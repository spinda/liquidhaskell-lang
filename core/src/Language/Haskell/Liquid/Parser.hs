{-# LANGUAGE TupleSections #-}

module Language.Haskell.Liquid.Parser (
    ParsedDec(..)
  , parseDecs
  , parseType
  ) where

-- TODO: Show code in error messages?

import           Prelude                          hiding (pred)

import           Control.Arrow
import           Control.Monad
import           Control.Monad.Trans

import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.String

import qualified Data.HashSet                     as S

import           Language.Haskell.TH              hiding (Pred)
import           Language.Haskell.TH.Syntax       hiding (Infix, Pred, lift)

import           System.IO

import           Text.Parsec                      hiding (parse)
import           Text.Parsec.Char
import           Text.Parsec.Combinator
import           Text.Parsec.Error
import           Text.Parsec.Expr
import           Text.Parsec.Language             (emptyDef)
import           Text.Parsec.Pos
import           Text.Parsec.Token                (GenLanguageDef (..))

import qualified Text.Parsec.Token                as T

import           Language.Haskell.Liquid.Fixpoint hiding (pAnd)
import           Language.Haskell.Liquid.RType

--------------------------------------------------------------------------------
-- Top-Level Entry Point -------------------------------------------------------
--------------------------------------------------------------------------------

data ParsedDec = TySyn Name [TyVarBndr] QuasiType
               | FnSig Name QuasiType

parseDecs :: String -> Q [ParsedDec]
parseDecs = parse $ many decP

parseType :: String -> Q (QuasiType, [Name])
parseType = parse typeP

--------------------------------------------------------------------------------
-- Parser Definition -----------------------------------------------------------
--------------------------------------------------------------------------------

type Parser = ParsecT String ParserState Q

parse :: Parser a -> String -> Q a
parse p src = do
  loc    <- location
  result <- go loc
  case result of
    Left err ->
      fail $ show $ startErrAt loc err
    Right result ->
      return result
  where
    go loc =
      runParserT (whiteSpace *> p <* eof) (PS loc mempty) (loc_filename loc) src

data ParserState = PS { ps_startLoc   :: Loc
                      , ps_implicitTV :: S.HashSet String
                      }

addImplicitTV :: String -> Parser ()
addImplicitTV tv = modifyState (\ps -> ps { ps_implicitTV = S.insert tv $ ps_implicitTV ps })

--------------------------------------------------------------------------------
-- Utility Functions -----------------------------------------------------------
--------------------------------------------------------------------------------

named :: String -> Parser a -> Parser a
named s p = p <?> s

startErrAt :: Loc -> ParseError -> ParseError
startErrAt loc err = setErrorPos errPos' err
  where
    (startLine, startCol) = loc_start loc

    errPos  = errorPos err
    errLine = sourceLine errPos
    errCol  = sourceColumn errPos

    errPos'  = setSourceLine (setSourceColumn errPos errCol') errLine'
    errLine' = errLine + startLine - 1
    errCol'
      | errLine == 1 =
        errCol + startCol - 1
      | otherwise =
        errCol

raiseErrAt :: SourcePos -> String -> Parser a
raiseErrAt pos err = do
  loc <- ps_startLoc <$> getState
  lift $ fail $ show $ startErrAt loc $ newErrorMessage (Message err) pos

unspacedIdent :: GenLanguageDef String ParserState Q -> Parser String
unspacedIdent def = try $ do
  name <- (:) <$> T.identStart def <*> many (T.identLetter def)
  if name `S.member` reservedSet
     then unexpected $ "reserved word " ++ show name
     else return name
  where
    reservedSet = S.fromList $ T.reservedNames def

--------------------------------------------------------------------------------
-- Language Definition ---------------------------------------------------------
--------------------------------------------------------------------------------

haskell :: T.GenTokenParser String ParserState Q
haskell = T.makeTokenParser haskellDef

haskellDef :: GenLanguageDef String ParserState Q
haskellDef = haskell98Def
  { identLetter   = identLetter haskell98Def <|> char '#'
  , reservedNames = reservedNames haskell98Def ++
      [ "foreign", "import", "export", "primitive"
      , "_ccall_", "_casm_"
      , "forall"
      ]
  }

haskell98Def :: GenLanguageDef String ParserState Q
haskell98Def = haskellStyle
  { reservedOpNames= ["::", "..", "=", "\\", "|", "<-", "->", "@", "~", "=>"]
  , reservedNames  = [ "let", "in", "case", "of", "if", "then", "else"
                     , "data", "type"
                     , "class", "default", "deriving", "do", "import"
                     , "infix", "infixl", "infixr", "instance", "module"
                     , "newtype", "where"
                     , "primitive"
                     ]
  }

haskellStyle :: GenLanguageDef String ParserState Q
haskellStyle = emptyDef
  { commentStart    = "{-"
  , commentEnd      = "-}"
  , commentLine     = "--"
  , nestedComments  = True
  , identStart      = letter
  , identLetter     = alphaNum <|> oneOf "_'"
  , opStart         = opLetter haskellStyle
  , opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , reservedOpNames = []
  , reservedNames   = []
  , caseSensitive   = True
  }


reserved, reservedOp :: String -> Parser ()
reserved   = T.reserved haskell
reservedOp = T.reservedOp haskell

varid, conid :: Parser String
varid = named "variable identifier" $ T.identifier $ T.makeTokenParser $ haskellDef { T.identStart = lower <|> char '_' }
conid = named "constructor identifier" $ T.identifier $ T.makeTokenParser $ haskellDef { T.identStart = upper              }

binder :: Parser String
binder = named "binder" $ T.identifier $ T.makeTokenParser $ haskellDef
  { T.identStart = lower <|> char '_'
  , T.reservedNames = T.reservedNames haskellDef ++ ["true", "false", "not", "mod"]
  }

whiteSpace :: Parser ()
whiteSpace = T.whiteSpace haskell

operator :: Parser String
operator = T.operator haskell

natural :: Parser Integer
natural = T.natural haskell

braces :: Parser a -> Parser a
braces = T.braces haskell

parens :: Parser a -> Parser a
parens = T.parens haskell

colon :: Parser String
colon = T.colon haskell

--------------------------------------------------------------------------------
-- Constructors and Vars -------------------------------------------------------
--------------------------------------------------------------------------------

data TyConOp = TyConOp SourcePos Name deriving (Show)

tyCon :: Parser (Either TyConOp Name)
tyCon = tyCon' "" <?> "type constructor"

tyCon' :: String -> Parser (Either TyConOp Name)
tyCon' prefix = (Left <$> tyConOp prefix) <|> tyConId prefix

tyConOp :: String -> Parser TyConOp
tyConOp prefix = do
  p  <- getPosition
  op <- operator
  return $ TyConOp p $ mkName $ prefix ++ op

tyConId :: String -> Parser (Either TyConOp Name)
tyConId prefix = do
  ident <- unspacedIdent $ haskellDef { T.identStart = upper }
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
  con     <- (lift . newName) =<< reserved "type" *> conid
  tvs     <- map (PlainTV . mkName) <$> many tyVar
  (ty, _) <- reservedOp "=" *> typeP
  return $ TySyn con tvs ty

fnSig :: Parser ParsedDec
fnSig = named "signature" $ do
  var       <- mkName <$> varid
  (ty, tvs) <- reservedOp "::" *> typeP
  return $ FnSig var $ quantifyRTy tvs ty

--------------------------------------------------------------------------------
-- LiquidHaskell Types ---------------------------------------------------------
--------------------------------------------------------------------------------

-- TODO: Move


typeP :: Parser (QuasiType, [Name])
typeP = do
  ty <- typeP' False
  ps <- getState
  putState $ ps { ps_implicitTV = mempty }
  return (ty, map mkName $ S.toList $ ps_implicitTV ps)

typeP' :: Bool -> Parser QuasiType
typeP' inParens = do
  bp <- getPosition
  b  <- optionMaybe $ try (binder <* colon)
  t1 <- arg
  t2 <- optionMaybe $ reservedOp "->" *> typeP' False
  case (t1, t2) of
    (_, Nothing) | isJust b ->
      raiseErrAt bp errBinderReturn
    (Left (TyConOp tp _), _) | not inParens || isJust t2 ->
      raiseErrAt tp errTyConOp
    (Left (TyConOp _ n), Nothing) ->
      return $ RApp n [] mempty
    (Right i, Just o) ->
      return $ RFun b i o mempty
    (Right t, Nothing) ->
      return t


arg :: Parser (Either TyConOp QuasiType)
arg = do
  args <- many1 arg'
  case args of
    [t] -> return t
    (Left (TyConOp p _) : _) ->
      raiseErrAt p errTyConOp
    (Right (RApp n as r) : re) -> do
      re' <- mapM go re
      return $ Right $ RApp n (as ++ re') mempty
    (Right ar : re ) ->
      fmap Right $ foldl' (\t1 t2 -> RAppTy t1 t2 mempty) ar <$> mapM go re
  where
    go (Left (TyConOp p _)) =
      raiseErrAt p errTyConOp
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
    go _ (Left (TyConOp tp _)) =
      raiseErrAt tp errTyConOp
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


errBinderReturn :: String
errBinderReturn =
  "Binders outside of {braces} cannot appear in a function's return type"

errTyConOp :: String
errTyConOp =
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

pops :: OperatorTable String ParserState Q Pred
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
    <|> (EVar <$> (binder <|> conid))

ite :: Parser Expr
ite = EIte <$> (reservedOp "if"   *> pred)
           <*> (reservedOp "then" *> expr)
           <*> (reservedOp "else" *> expr)

eops :: OperatorTable String ParserState Q Expr
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

