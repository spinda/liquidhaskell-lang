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

import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.String

import qualified Data.HashSet                     as S

import           Language.Haskell.TH              hiding (Pred, appT)
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
parseDecs simpl = parse simpl $ concat <$> many decP

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
     , ps_exprParams :: S.HashSet String
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
      runParserT (whiteSpace *> p <* eof) (PS loc simpl mempty mempty) (loc_filename loc) src

addImplicitTV :: String -> Parser ()
addImplicitTV tv =
  modifyState (\ps -> ps { ps_implicitTV = S.insert tv $ ps_implicitTV ps })

drainImplicitTVs :: Parser [Name]
drainImplicitTVs = do
  ps <- getState
  putState $ ps { ps_implicitTV = mempty }
  return $ map mkName $ S.toList $ ps_implicitTV ps

addExprParams :: [String] -> Parser ()
addExprParams params =
  modifyState (\ps -> ps { ps_exprParams = S.union (S.fromList params) $ ps_exprParams ps })

isExprArg :: String -> Parser Bool
isExprArg param = do
  params <- ps_exprParams <$> getState
  return (param `S.member` params)

pickSimpl :: a -> a -> Parser a
pickSimpl x y = do
  simpl <- ps_buildSimpl <$> getState
  return $ if simpl then x else y

--------------------------------------------------------------------------------
-- Utility Functions -----------------------------------------------------------
--------------------------------------------------------------------------------

named :: String -> Parser a -> Parser a
named s p = p <?> s

withPos :: Parser a -> Parser (SourcePos, a)
withPos p = (,) <$> getPosition <*> p

withSpan :: Parser a -> Parser (Span, a)
withSpan p = do
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

mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f = liftM catMaybes . mapM f

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

comma :: Parser String
comma = T.comma haskell

--------------------------------------------------------------------------------
-- Constructors and Vars -------------------------------------------------------
--------------------------------------------------------------------------------

data TyCon = TyCon { tc_op   :: Bool
                   , tc_name :: Name
                   }


tyCon :: Parser TyCon
tyCon = tyCon' "" <?> "type constructor"

tyCon' :: String -> Parser TyCon
tyCon' prefix = tyConOp prefix <|> tyConId prefix

tyConOp :: String -> Parser TyCon
tyConOp prefix = do
  p  <- getPosition
  op <- operator
  return $ TyCon True $ mkName $ prefix ++ op

tyConId :: String -> Parser TyCon
tyConId prefix = do
  ident <- unspacedIdent $ haskellDef { T.identStart = upper }
  (char '.' *> tyCon' (prefix ++ ident ++ ".")) <|> (TyCon False (mkName $ prefix ++ ident) <$ whiteSpace)


tyVar :: Parser String
tyVar = varid <?> "type variable"

exprParam :: Parser String
exprParam = varid <?> "expression parameter"

--------------------------------------------------------------------------------
-- Declarations ----------------------------------------------------------------
--------------------------------------------------------------------------------

decP :: Parser [Dec]
decP = tySyn <|> fnSig


tySyn :: Parser [Dec]
tySyn = named "type synonym" $ do
  con     <- (lift . newName) =<< reserved "type" *> conid
  evs     <- exprParams
  tvs     <- map (PlainTV . mkName) <$> many (tyVar <|> (lookAhead exprParams *> fail errExprParamsPos))
  (ty, _) <- reservedOp "=" *> typeP
  let tySynD = TySynD con tvs ty
  if null evs
     then return [tySynD]
     else pickSimpl [tySynD] [tySynD, annExprParams con evs]

exprParams :: Parser [String]
exprParams = do
  evs <- braces (many1 ((,) <$> getPosition <*> exprParam)) <|> return []
  foldM checkUnique [] evs
  where
    checkUnique seen (p, param)
      | param `elem` seen = raiseErrAt p $ errDupExprParam param
      | otherwise         = return (param:seen)


fnSig :: Parser [Dec]
fnSig = named "signature" $ do
  var       <- mkName <$> varid
  (ty, tvs) <- reservedOp "::" *> typeP
  return [SigD var ty]

--------------------------------------------------------------------------------
-- LiquidHaskell-Annotated Types -----------------------------------------------
--------------------------------------------------------------------------------

data Func = Func (Maybe (Span, String)) [(SourcePos, Term)] (Maybe Func)

data Term = OpTerm Name
          | TcTerm Name
          | TvTerm Name
          | ExTerm Span [Expr]
          | GrTerm Func
          | ReTerm String Func Reft


typeFromFunc :: Func -> Parser Type
typeFromFunc (Func b ts f) = do
  ty  <- typeFromTerms ts
  ty' <- case b of
    Nothing     -> return ty
    Just (p, x) -> pickSimpl ty $ bind p x ty
  case f of
    Nothing -> return ty'
    Just f' -> (funT ty') <$> typeFromFunc f'


typeFromTerms :: [(SourcePos, Term)] -> Parser Type

typeFromTerms ((p, OpTerm _) : _) =
  raiseErrAt p errTyConOp

typeFromTerms ((_, TcTerm name) : (_, ExTerm span es) : terms) = do
  terms' <- mapM typeFromTerm terms
  let app = appT (ConT name) terms'
  pickSimpl app $ exprArgs app span es

typeFromTerms ((_, TcTerm name) : terms) = do
  appT (ConT name) <$> mapM typeFromTerm terms

typeFromTerms ((_, TvTerm name) : terms) = do
  terms' <- mapM typeFromTerm terms
  appT (VarT name) <$> mapM typeFromTerm terms

typeFromTerms ((p, ExTerm _ _) : _) =
  raiseErrAt p errExprArgsPos

typeFromTerms ((_, GrTerm (Func b [(p, OpTerm name)] f)) : terms) = do
  t <- typeFromFunc $ Func b [(p, TcTerm name)] f
  appT t <$> mapM typeFromTerm terms

typeFromTerms ((_, GrTerm f) : terms) = do
  t <- typeFromFunc f
  appT t <$> mapM typeFromTerm terms

typeFromTerms ((_, ReTerm b f r) : terms) = do
  t      <- typeFromFunc f
  terms' <- mapM typeFromTerm terms
  pickSimpl t $ refine t b r


typeFromTerm :: (SourcePos, Term) -> Parser Type
typeFromTerm t = typeFromTerms [t]


typeP :: Parser (Type, [Name])
typeP = do
  ty  <- typeP'
  tvs <- drainImplicitTVs
  return (ty, tvs)

typeP' :: Parser Type
typeP' = typeFromFunc =<< funcP

funcP :: Parser Func
funcP = do
  b <- optionMaybe $ try (withSpan binder <* colon)
  t <- many1 (withPos termP)
  f <- optionMaybe $ reservedOp "->" *> funcP
  return $ Func b t f


termP :: Parser Term
termP = (GrTerm <$> parens funcP)
    <|> braces (refinedP <|> exprArgsP)
    <|> tyconTermP
    <|> tvTermP

refinedP :: Parser Term
refinedP = do
  b <- try (binder <* colon)
  f <- funcP
  r <- reservedOp "|" *> reft
  return $ ReTerm b f r

exprArgsP :: Parser Term
exprArgsP = uncurry ExTerm <$> withSpan (sepBy1 expr comma)

tyconTermP :: Parser Term
tyconTermP = do
  tc <- tyCon
  return $ if tc_op tc
    then OpTerm $ tc_name tc
    else TcTerm $ tc_name tc

tvTermP :: Parser Term
tvTermP = do
  tv <- tyVar
  addImplicitTV tv
  return $ TvTerm $ mkName tv


errTyConOp :: String
errTyConOp =
  "Type constructor operators must be surrounded in (parentheses)"

errExprArgsPos :: String
errExprArgsPos =
  "Expression argument list can only appear immediately after a type constructor"

errExprParamsPos :: String
errExprParamsPos =
  "Expression parameters list must appear only once, before any type variables"

errDupExprParam :: String -> String
errDupExprParam param =
  "Duplicate expression parameter \"" ++ param ++ "\""

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
    <|> evar
    <|> (uncurry eCtr <$> withSpan conid)

evar :: Parser Expr
evar = do
  v <- binder <?> "binder or expression argument"
  genExprArg <- isExprArg v
  return $
    if genExprArg
       then eArg v
       else eBdr v

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

