{-# LANGUAGE TupleSections #-}

module Language.Haskell.Liquid.Parse.Type (
    typeP
  ) where

import Control.Monad.State

import qualified Data.HashSet as S

import Language.Haskell.TH.Syntax hiding (lift)

import Text.Parsec

import Language.Haskell.Liquid.Build

import Language.Haskell.Liquid.Parse.Base
import Language.Haskell.Liquid.Parse.Reft

--------------------------------------------------------------------------------
-- Top-Level Entry Point -------------------------------------------------------
--------------------------------------------------------------------------------

typeP :: Parser ([Name], Type)
typeP = parseType =<< lexType

--------------------------------------------------------------------------------
-- Lexing Phase ----------------------------------------------------------------
--------------------------------------------------------------------------------

data Term = OpTerm Name
          | TcTerm Name
          | TvTerm String
          | ExTerm Span Expr
          | GrTerm [(SourcePos, Term)]
          | BdTerm [(SourcePos, Term)] Span String
          | ReTerm [(SourcePos, Term)] String Reft
          | FnTerm


lexType :: Parser [(SourcePos, Term)]
lexType = concat <$> many1 (lexType' <|> (return <$> withPos (FnTerm <$ reservedOp "->")))

lexType' :: Parser [(SourcePos, Term)]
lexType' = do
  p <- getPosition
  b <- optionMaybe $ try (withSpan binderP <* colon)
  t <- many1 termP
  return $ case b of
    Nothing     -> t
    Just (s, x) -> [(p, BdTerm t s x)]


termP :: Parser (SourcePos, Term)
termP = withPos termP'

termP' :: Parser Term
termP' = parens (GrTerm <$> lexType)
     <|> braces (reTermP <|> exTermP)
     <|> tcTermP
     <|> tvTermP


reTermP :: Parser Term
reTermP = do
  b <- try (binderP <* colon)
  t <- lexType
  r <- reservedOp "|" *> reftP
  return $ ReTerm t b r

exTermP :: Parser Term
exTermP = uncurry ExTerm <$> withSpan exprP


tcTermP :: Parser Term
tcTermP = uncurry go =<< withSpan tyConP
  where
    go span (TyCon op id)
      | op = return $ OpTerm $ mkName id
      | otherwise = do
        genExprParam <- isExprParam id
        return $ if genExprParam
          then ExTerm span $ eParam id
          else TcTerm $ mkName id

tvTermP :: Parser Term
tvTermP = TvTerm <$> tyVarP

--------------------------------------------------------------------------------
-- Parsing Phase ---------------------------------------------------------------
--------------------------------------------------------------------------------

type TypeParser = StateT ParserState (Either (SourcePos, String))

data ParserState = PS { ps_simplified  :: Bool
                      , ps_implicitTVs :: S.HashSet String
                      }

failure :: SourcePos -> String -> TypeParser a
failure p err = lift $ Left (p, err)

annotate :: (a -> a) -> a -> TypeParser a
annotate f x = do
  simplified <- gets ps_simplified
  return $ if simplified
    then x
    else f x

addImplicitTV :: String -> TypeParser ()
addImplicitTV tv =
  modify (\ps -> ps { ps_implicitTVs = S.insert tv $ ps_implicitTVs ps })


data Arg = TyArg Type
         | ExArg Span Expr
         | FnArg


parseType :: [(SourcePos, Term)] -> Parser ([Name], Type)
parseType terms = do
  simplified <- getSimplified
  either (uncurry raiseErrAt) finalize $
    runStateT (parseType' terms) (PS simplified mempty)
  where
    finalize (ty, PS _ tvs) = return (map mkName $ S.toList tvs, ty)

parseType' :: [(SourcePos, Term)] -> TypeParser Type
parseType' terms = do
  args <- mapM ofTerm terms
  ofArgs Nothing args


ofTerm :: (SourcePos, Term) -> TypeParser (SourcePos, Arg)

ofTerm (p, OpTerm _) =
  failure p errTyConOp

ofTerm (p, TcTerm name) =
  return (p, TyArg $ ConT name)
ofTerm (p, TvTerm id) = do
  addImplicitTV id
  return (p, TyArg $ VarT $ mkName id)

ofTerm (p, ExTerm span e) =
  return (p, ExArg span e)

ofTerm (_, GrTerm [(p, OpTerm name)]) =
  ofTerm (p, TcTerm name)
ofTerm (_, GrTerm [(p, ExTerm span e)]) =
  ofTerm (p, ExTerm span e)
ofTerm (_, GrTerm [(p, FnTerm)]) =
  return (p, TyArg ArrowT)
ofTerm (p, GrTerm terms) =
  ((p, ) . TyArg) <$> parseType' terms

ofTerm (p, BdTerm terms span bndr) =
  ((p, ) . TyArg) <$> (annotate (bind span bndr) =<< parseType' terms)
ofTerm (p, ReTerm terms bndr rft) =
  ((p, ) . TyArg) <$> (annotate (refine bndr rft) =<< parseType' terms)

ofTerm (p, FnTerm) =
  return (p, FnArg)


ofArgs :: Maybe Type -> [(SourcePos, Arg)] -> TypeParser Type

ofArgs Nothing [] =
  error "Language.Haskell.Liquid.Parse.Type.ofArgs called on invalid input"
ofArgs (Just ty) [] =
  return ty

ofArgs Nothing ((_, TyArg ty) : rest) =
  ofArgs (Just ty) rest
ofArgs (Just t1) ((_, TyArg t2) : rest) =
  ofArgs (Just (t1 `AppT` t2)) rest

ofArgs Nothing [(p, ExArg _ _)] =
  failure p errExprArgAlone
ofArgs Nothing ((p, ExArg _ _) : _) =
  failure p errExprArgHead
ofArgs (Just ty) ((_, ExArg span e) : rest) = do
  es <- mapM go rest
  annotate (exprArgs ((span, e) : es)) ty
  where
    go (_, ExArg span e) = return (span, e)
    go (p, _           ) = failure p errExprArgTail

ofArgs Nothing ((p, FnArg) : _) =
  failure p errTyConOp
ofArgs (Just ty) ((_, FnArg) : rest) =
  funT ty <$> ofArgs Nothing rest

--------------------------------------------------------------------------------
-- Error Messages --------------------------------------------------------------
--------------------------------------------------------------------------------

errTyConOp :: String
errTyConOp =
  "Type constructor operators must be surrounded in (parentheses)"

errExprArgAlone :: String
errExprArgAlone =
  "Expression argument cannot appear alone like this"

errExprArgHead :: String
errExprArgHead =
  "Expression arguments cannot start a type application"

errExprArgTail :: String
errExprArgTail =
  "Type applications cannot follow expression arguments: expression arguments must be at the end"

