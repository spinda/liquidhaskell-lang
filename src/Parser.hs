{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module Parser (ParsedDec(..), quantify, parseDecs, parseType) where

-- TODO: Because Trifecta pulls in so many dependencies, the parser will most
--       likely be switched to Parsec. Fortunately, their APIs are similar
--       enough and the pareser is lean enough that this shouldn't be too much
--       of a hassle.

import Prelude hiding (pred)

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.State

import Data.List
import Data.Maybe
import Data.Monoid
import Data.String

import qualified Data.HashSet         as S
import qualified Data.ByteString.UTF8 as U

import Language.Haskell.TH hiding (Pred)
import Language.Haskell.TH.Syntax hiding (Infix, Pred, lift)

import System.IO

import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.Expression hiding (buildExpressionParser)
import Text.Parser.Token
import Text.Parser.Token.Highlight
import Text.Parser.Token.Style

import qualified Text.PrettyPrint.ANSI.Leijen as LJ

import qualified Text.Trifecta as T
import qualified Text.Trifecta.Delta as T

import Fixpoint hiding (pAnd)
import RType
import Util

--------------------------------------------------------------------------------
-- Top-Level Entry Point -------------------------------------------------------
--------------------------------------------------------------------------------

data ParsedDec = TySyn String [TyVarBndr] QuasiType
               | FnSig Name QuasiType

quantify :: [Name] -> QuasiType -> QuasiType
quantify tvs ty = foldr RAllT ty $ reverse tvs

parseDecs :: String -> Either String [ParsedDec]
parseDecs = parse $ many decP

parseType :: String -> Either String (QuasiType, [Name])
parseType = parse typeP

parse :: Parser a -> String -> Either String a
parse p src =
  case result of
    T.Failure xs ->
      Left $ flip LJ.displayS "" $ LJ.renderPretty 0.8 80 $ xs <> LJ.linebreak
    T.Success (x, _) ->
      Right x
  where
    result =
      T.parseString (runStateT (runParser (whiteSpace *> p <* eof)) mempty) mempty src 

--------------------------------------------------------------------------------
-- Parser Definition -----------------------------------------------------------
--------------------------------------------------------------------------------

newtype Parser a = Parser { runParser :: StateT ParserState T.Parser a }
                   deriving ( Functor, Applicative, Alternative
                            , Monad, MonadPlus, MonadState ParserState
                            , Parsing, CharParsing, T.DeltaParsing, T.MarkParsing T.Delta
                            )

instance T.Errable Parser where
  raiseErr = Parser . lift . T.raiseErr

instance TokenParsing Parser where
  someSpace = buildSomeSpaceParser (skipSome space) haskellCommentStyle


data ParserState = PS { ps_implicitTV :: S.HashSet String
                      }

instance Monoid ParserState where
  mempty = PS mempty
  mappend (PS xs) (PS ys) = PS $ mappend xs ys
  mconcat xs = PS $ mconcat (map ps_implicitTV xs)

addImplicitTV :: String -> Parser ()
addImplicitTV tv = modify (\ps -> ps { ps_implicitTV = S.insert tv $ ps_implicitTV ps })

--------------------------------------------------------------------------------
-- Utility Functions -----------------------------------------------------------
--------------------------------------------------------------------------------

named :: String -> Parser a -> Parser a
named n p = p <?> n

unspacedIdent :: (TokenParsing m, Monad m, IsString s) => IdentifierStyle m -> m s 
unspacedIdent s = fmap fromString $ try $ do
  name <- highlight (_styleHighlight s) ((:) <$> _styleStart s <*> many (_styleLetter s) <?> _styleName s)
  when (S.member name (_styleReserved s)) $ unexpected $ "reserved " ++ _styleName s ++ " " ++ show name
  return name

raiseErrAt :: (T.Errable m, T.MarkParsing d m) => d -> T.Err -> m a
raiseErrAt m e = T.release m >> T.raiseErr e

--------------------------------------------------------------------------------
-- Language Definition ---------------------------------------------------------
--------------------------------------------------------------------------------

reserved, reservedOp :: String -> Parser ()
reserved   = reserve haskellIdents
reservedOp = reserve haskellOps


varStyle, conStyle :: IdentifierStyle Parser
varStyle = haskellIdents { _styleName      = "variable identifier"
                         , _styleStart     = lower <|> char '_' 
                         , _styleHighlight = Identifier
                         }
conStyle = haskellIdents { _styleName      = "constructor identifier"
                         , _styleStart     = upper 
                         , _styleHighlight = Constructor
                         }


varid, conid :: Parser String
varid  = ident varStyle
conid  = ident conStyle

binder :: Parser String
binder = ident $ varStyle { _styleName = "binder"
                          , _styleReserved = S.union (S.fromList ["true", "false", "not", "mod"]) (_styleReserved varStyle)
                          }

--------------------------------------------------------------------------------
-- Constructors and Vars -------------------------------------------------------
--------------------------------------------------------------------------------

data TyConOp = TyConOp T.Delta Name deriving (Show)

tyCon :: Parser (Either TyConOp Name)
tyCon = tyCon' "" <?> "type constructor"

tyCon' :: String -> Parser (Either TyConOp Name)
tyCon' prefix = (Left <$> tyConOp prefix) <|> tyConId prefix

tyConOp :: String -> Parser TyConOp
tyConOp prefix = do
  m  <- T.mark
  op <- ident haskellOps <?> "type constructor operator"
  return $ TyConOp m $ mkName $ prefix ++ op

tyConId :: String -> Parser (Either TyConOp Name)
tyConId prefix = do
  ident <- unspacedIdent $ conStyle { _styleName = "type constructor ident or module name" }
  (char '.' *> tyCon' (prefix ++ ident ++ ".")) <|> (Right (mkName $ prefix ++ ident) <$ whiteSpace)


tyVar :: Parser String
tyVar = varid <?> "type variable"

--------------------------------------------------------------------------------
-- Declarations ----------------------------------------------------------------
--------------------------------------------------------------------------------

decP :: Parser ParsedDec
decP = tySyn <|> fnSig

tySyn :: Parser ParsedDec
tySyn = named "type synonym" $ do
  con     <- reserved "type" *> conid
  tvs     <- map (PlainTV . mkName) <$> many tyVar
  (ty, _) <- reservedOp "=" *> typeP
  return $ TySyn con tvs ty

fnSig :: Parser ParsedDec
fnSig = named "signature" $ do
  var       <- mkName <$> varid
  (ty, tvs) <- reservedOp "::" *> typeP
  return $ FnSig var $ quantify tvs ty

--------------------------------------------------------------------------------
-- LiquidHaskell Types ---------------------------------------------------------
--------------------------------------------------------------------------------

typeP :: Parser (QuasiType, [Name])
typeP = do
  ty     <- typeP' False
  PS tvs <- get
  put mempty
  return (ty, map mkName $ S.toList tvs)

typeP' :: Bool -> Parser QuasiType
typeP' inParens = do
  bm <- T.mark
  b  <- optional $ try (binder <* colon)
  t1 <- arg
  t2 <- optional $ reservedOp "->" *> typeP' False
  case (t1, t2) of
    (_, Nothing) | isJust b ->
      raiseErrAt bm errBinderReturn
    (Left (TyConOp tm _), _) | not inParens || isJust t2 ->
      raiseErrAt tm errTyConOp
    (Left (TyConOp _ n), Nothing) ->
      return $ RApp n [] mempty
    (Right i, Just o) ->
      return $ RFun b i o mempty
    (Right t, Nothing) ->
      return t


arg :: Parser (Either TyConOp QuasiType)
arg = do
  args <- some arg'
  case args of
    [t] -> return t
    (Left (TyConOp m _) : _) ->
      raiseErrAt m errTyConOp
    (Right (RApp n as r) : re) -> do
      re' <- mapM go re
      return $ Right $ RApp n (as ++ re') mempty
    (Right ar : re ) ->
      fmap Right $ foldl' (\t1 t2 -> RAppTy t1 t2 mempty) ar <$> mapM go re
  where
    go (Left (TyConOp m _)) =
      raiseErrAt m errTyConOp
    go (Right ar) =
      return ar

arg' :: Parser (Either TyConOp QuasiType)
arg' = (Right <$> refined)
   <|> (Right <$> parens (typeP' True))
   <|> (Right <$> tyVarArg)
   <|> tyConArg

refined :: Parser QuasiType
refined = braces $ do
  b <- binder <* colon
  t <- arg
  r <- reservedOp "|" *> reft b
  go r t
  where
    go _ (Left (TyConOp tm _)) =
      raiseErrAt tm errTyConOp
    go r (Right (RVar tv r')) =
      return $ RVar tv (mappend r r')
    go r (Right (RAppTy t1 t2 r')) =
      return $ RAppTy t1 t2 (mappend r r')
    go r (Right (RApp tc as r')) =
      return $ RApp tc as (mappend r r')
    go r (Right (RFun b i o r')) =
      return $ RFun b i o (mappend r r')
    go r (Right (RAllT tv ty)) =
      RAllT tv <$> go r (Right ty)

tyVarArg :: Parser QuasiType
tyVarArg = do
  tv <- tyVar
  addImplicitTV tv
  return $ RVar (mkName tv) mempty

tyConArg :: Parser (Either TyConOp QuasiType)
tyConArg = fmap (\n -> RApp n [] mempty) <$> tyCon


errBinderReturn :: T.Err
errBinderReturn = T.failed
  "Binders outside of {braces} cannot appear in a function's return type"

errTyConOp :: T.Err
errTyConOp = T.failed
  "Type constructor operators must be surrounded in (parentheses)"

--------------------------------------------------------------------------------
-- Reft ------------------------------------------------------------------------
--------------------------------------------------------------------------------

reft :: String -> Parser RReft
reft b = Reft . (b, ) <$> refa

refa :: Parser Refa
refa = Refa <$> pred

--------------------------------------------------------------------------------
-- Pred ------------------------------------------------------------------------
--------------------------------------------------------------------------------

pred :: Parser Pred
pred = buildExpressionParser pops pterm <?> "predicate"

pterm :: Parser Pred
pterm = parens pred
    <|> (PTrue  <$ reserved "true")
    <|> (PFalse <$ reserved "false")
    <|> expOrAtom

expOrAtom :: Parser Pred
expOrAtom = do
  e1 <- expr
  option (PBexp e1) $ do
    op <- brel
    e2 <- expr
    return $ PAtom op e1 e2

pops :: OperatorTable Parser Pred
pops = [ [Prefix (PNot <$ reservedOp "~"  )           ]
       , [Prefix (PNot <$ reserved   "not")           ]
       , [Infix  (pAnd <$ reservedOp "&&" ) AssocRight]
       , [Infix  (pOr  <$ reservedOp "||" ) AssocRight]
       , [Infix  (PImp <$ reservedOp "=>" ) AssocRight]
       , [Infix  (PImp <$ reservedOp "==>") AssocRight]
       , [Infix  (PIff <$ reservedOp "<=>") AssocRight]
       ]

pAnd :: Pred -> Pred -> Pred
pAnd (PAnd ps1) (PAnd ps2) = PAnd (ps1 ++ ps2)
pAnd p          (PAnd ps)  = PAnd (p:ps)
pAnd (PAnd ps)  p          = PAnd (ps ++ [p])
pAnd p1         p2         = PAnd [p1, p2]

pOr :: Pred -> Pred -> Pred
pOr (POr ps1) (POr ps2) = POr (ps1 ++ ps2)
pOr p         (POr ps)  = POr (p:ps)
pOr (POr ps)  p         = POr (ps ++ [p])
pOr p1        p2        = POr [p1, p2]

brel :: Parser Brel
brel = Eq  <$ reservedOp "=="
   <|> Eq  <$ reservedOp "="
   <|> Ne  <$ reservedOp "!="
   <|> Ne  <$ reservedOp "/="
   <|> Une <$ reservedOp "!~"
   <|> Lt  <$ reservedOp "<"
   <|> Le  <$ reservedOp "<="
   <|> Gt  <$ reservedOp ">"
   <|> Ge  <$ reservedOp ">="

--------------------------------------------------------------------------------
-- Expr ------------------------------------------------------------------------
--------------------------------------------------------------------------------

expr :: Parser Expr
expr = buildExpressionParser eops eterm <?> "expression"

eterm :: Parser Expr
eterm = parens expr
    <|> (EBot <$ reservedOp "_|_")
    <|> ite
    <|> (ECon . I <$> natural)
    <|> (EVar <$> binder)

ite :: Parser Expr
ite = EIte <$> (reservedOp "if"   *> pred)
           <*> (reservedOp "then" *> expr)
           <*> (reservedOp "else" *> expr)

eops :: OperatorTable Parser Expr
eops = [ [ Prefix (ENeg <$ reservedOp "-")
         ]
       , [ Infix (EBin Times <$ reservedOp "*"  ) AssocLeft
         , Infix (EBin Div   <$ reservedOp "/"  ) AssocLeft
         ]
       , [ Infix (EBin Minus <$ reservedOp "-"  ) AssocLeft
         , Infix (EBin Plus  <$ reservedOp "+"  ) AssocLeft
         ]
       , [ Infix (EBin Mod   <$ reserved   "mod") AssocLeft
         ]
       ]

