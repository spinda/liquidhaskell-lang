{-# LANGUAGE TupleSections #-}

module Language.Haskell.Liquid.Parse (
    -- * Top-Level Entry Points
    parseDecs
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

import           Language.Haskell.Liquid.Build
import           Language.Haskell.Liquid.RType    (Brel(..), Bop(..))
import           Language.Haskell.Liquid.Util

--------------------------------------------------------------------------------
-- Top-Level Entry Points ------------------------------------------------------
--------------------------------------------------------------------------------

parseDecs :: Bool -> String -> Q [Dec]
parseDecs simpl = parse simpl $ many decP

parseType :: Bool -> String -> Q (Type, [Name])
parseType simpl = parse simpl typeP

--------------------------------------------------------------------------------
-- Parser Definition -----------------------------------------------------------
--------------------------------------------------------------------------------

type Parser = ParsecT String ParserState Q

data ParserState =
  PS { ps_startLoc   :: Loc
     , ps_buildSimpl :: Bool
     , ps_implicitTV :: S.HashSet String
     }


parse :: Bool -> Parser a -> String -> Q a
parse simpl p src = do
  loc    <- location
  result <- go loc
  case result of
    Left err ->
      fail $ show $ startErrAt loc err
    Right result ->
      return result
  where
    go loc =
      runParserT (whiteSpace *> p <* eof) (PS loc simpl mempty) (loc_filename loc) src

addImplicitTV :: String -> Parser ()
addImplicitTV tv =
  modifyState (\ps -> ps { ps_implicitTV = S.insert tv $ ps_implicitTV ps })

pickSimpl :: a -> a -> Parser a
pickSimpl x y = do
  simpl <- ps_buildSimpl <$> getState
  return $ if simpl then x else y

--------------------------------------------------------------------------------
-- Utility Functions -----------------------------------------------------------
--------------------------------------------------------------------------------

named :: String -> Parser a -> Parser a
named s p = p <?> s

located :: Parser a -> Parser (Span, a)
located p = do
  s <- getPosition
  x <- p
  e <- getPosition -- TODO: End position minus whitespace!
  return (mkSpan s e, x)

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

decP :: Parser Dec
decP = tySyn <|> fnSig

tySyn :: Parser Dec
tySyn = named "type synonym" $ do
  con     <- (lift . newName) =<< reserved "type" *> conid
  tvs     <- map (PlainTV . mkName) <$> many tyVar
  (ty, _) <- reservedOp "=" *> typeP
  return $ TySynD con tvs ty

fnSig :: Parser Dec
fnSig = named "signature" $ do
  var       <- mkName <$> varid
  (ty, tvs) <- reservedOp "::" *> typeP
  return $ SigD var $ quantifyTy tvs ty

--------------------------------------------------------------------------------
-- LiquidHaskell-Annotated Types -----------------------------------------------
--------------------------------------------------------------------------------

typeP :: Parser (Type, [Name])
typeP = do
  ty <- typeP' False
  ps <- getState
  putState $ ps { ps_implicitTV = mempty }
  return (ty, map mkName $ S.toList $ ps_implicitTV ps)

typeP' :: Bool -> Parser Type
typeP' inParens = do
  bp <- getPosition
  bm <- optionMaybe $ try (located binder <* colon)
  t1 <- arg
  t2 <- optionMaybe $ reservedOp "->" *> typeP' False
  case (bm, t1, t2) of
    (_, Left (TyConOp p _), _) | not inParens || isJust t2 ->
      raiseErrAt p errTyConOp
    (Nothing, Left (TyConOp _ n), Nothing) ->
      return $ ConT n
    (Nothing, Right i, Nothing) ->
      return i
    (Nothing, Right i, Just o) ->
      return $ funT i o
    (Just (span, b), Right i, Nothing) ->
      pickSimpl i $ bind span b i
    (Just (span, b), Right i, Just o) ->
      pickSimpl (funT i o) $ funT (bind span b i) o


arg :: Parser (Either TyConOp Type)
arg = do
  a <- many1 arg'
  case a of
    [t] ->
      return t
    Left (TyConOp p _) : _ ->
      raiseErrAt p errTyConOp
    Right t : ts -> do
      ts' <- mapM go ts
      return $ Right $ case t of
        AppT t1 t2 -> AppT t1 $ foldl' AppT t2 ts'
        _          ->           foldl' AppT t  ts'
  where
    go (Left (TyConOp p _)) =
      raiseErrAt p errTyConOp
    go (Right t) =
      return t

arg' :: Parser (Either TyConOp Type)
arg' =
      (Right <$> refined)
  <|> (Right <$> parens (typeP' True))
  <|> (Right <$> tyVarArg)
  <|> tyConArg


refined :: Parser Type
refined = braces $ do
  b <- binder <* colon
  a <- arg
  case a of
    Left (TyConOp p _) ->
      raiseErrAt p errTyConOp
    Right t ->
      option t $ do
        r <- reservedOp "|" *> reft
        pickSimpl t $ refine t b r

tyVarArg :: Parser Type
tyVarArg = do
  tv <- tyVar
  addImplicitTV tv
  return $ VarT $ mkName tv

tyConArg :: Parser (Either TyConOp Type)
tyConArg = fmap ConT <$> tyCon


errTyConOp :: String
errTyConOp =
  "Type constructor operators must be surrounded in (parentheses)"

--------------------------------------------------------------------------------
-- Reft ------------------------------------------------------------------------
--------------------------------------------------------------------------------

reft :: Parser Reft
reft = rPred <$> pred

--------------------------------------------------------------------------------
-- Pred ------------------------------------------------------------------------
--------------------------------------------------------------------------------

pred :: Parser Pred
pred = buildExpressionParser pops pterm <?> "predicate"

pterm :: Parser Pred
pterm = parens pred
    <|> (pTrue  <$ reserved "true")
    <|> (pFalse <$ reserved "false")
    <|> expOrAtom

expOrAtom :: Parser Pred
expOrAtom = do
  e1 <- expr
  option (pExp e1) $ do
    op <- brel
    e2 <- expr
    return $ pAtom op e1 e2

pops :: OperatorTable String ParserState Q Pred
pops = [ [Prefix (pNot <$ reservedOp "~"  )           ]
       , [Prefix (pNot <$ reserved   "not")           ]
       , [Infix  (pAnd <$ reservedOp "&&" ) AssocRight]
       , [Infix  (pOr  <$ reservedOp "||" ) AssocRight]
       , [Infix  (pImp <$ reservedOp "=>" ) AssocRight]
       , [Infix  (pImp <$ reservedOp "==>") AssocRight]
       , [Infix  (pIff <$ reservedOp "<=>") AssocRight]
       ]

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
    <|> (eBot <$ reservedOp "_|_")
    <|> ite
    <|> (eConNat <$> natural)
    <|> (eBdr <$> binder)
    <|> (uncurry eCtr <$> located conid)

ite :: Parser Expr
ite = eIte <$> (reservedOp "if"   *> pred)
           <*> (reservedOp "then" *> expr)
           <*> (reservedOp "else" *> expr)

eops :: OperatorTable String ParserState Q Expr
eops = [ [ Prefix (eNeg <$ reservedOp "-")
         ]
       , [ Infix (eBin Times <$ reservedOp "*"  ) AssocLeft
         , Infix (eBin Div   <$ reservedOp "/"  ) AssocLeft
         ]
       , [ Infix (eBin Minus <$ reservedOp "-"  ) AssocLeft
         , Infix (eBin Plus  <$ reservedOp "+"  ) AssocLeft
         ]
       , [ Infix (eBin Mod   <$ reserved   "mod") AssocLeft
         ]
       ]

