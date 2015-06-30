module Language.Haskell.Liquid.Parse.Base (
    -- * Parser Type
    Parser
  , runParser
  , getSimplified
  , ifSimplified
  , addExprParams
  , isExprParam

    -- * Error Messages
  , raiseErrAt

    -- * Parsing Utility Functions
  , named
  , located
  , withPos
  , unspacedIdent

    -- * Language Definition
  , reserved, reservedOp, operator
  , whiteSpace
  , natural
  , braces, parens
  , colon, comma

    -- * Identifier Parsers
  , varidP, conidP, binderP
  , tyConP, TyCon(..)
  , tyVarP, exprParamP
  , fTyConP
  ) where

import Control.Monad.Trans

import qualified Data.HashSet as S

import Language.Haskell.TH.Syntax hiding (lift)

import Text.Parsec hiding (runParser)
import Text.Parsec.Error
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Pos
import Text.Parsec.Token (GenLanguageDef(..))

import qualified Text.Parsec.Token as T

import Language.Haskell.Liquid.Build
import Language.Haskell.Liquid.RType (FTycon(..))

--------------------------------------------------------------------------------
-- Parser Type -----------------------------------------------------------------
--------------------------------------------------------------------------------

type Parser = ParsecT String ParserState Q

data ParserState = PS { ps_startLoc   :: Loc
                      , ps_simplified :: Bool
                      , ps_exprParams :: S.HashSet String
                      }

runParser :: Bool -> Parser a -> String -> Q a
runParser simplified p src = do
  loc    <- location
  result <- go loc
  case result of
    Left  err    -> failWithError loc err
    Right result -> return result
  where
    go loc =
      runParserT (whiteSpace *> p <* eof) (PS loc simplified mempty) (loc_filename loc) src

getSimplified :: Parser Bool
getSimplified = ps_simplified <$> getState
{-# INLINE getSimplified #-}

ifSimplified :: a -> a -> Parser a
ifSimplified x y = do
  simplified <- getSimplified
  return $ if simplified
    then x
    else y

addExprParams :: [String] -> Parser ()
addExprParams params = do
  modifyState (\ps -> ps { ps_exprParams = S.union (S.fromList params) $ ps_exprParams ps })

isExprParam :: String -> Parser Bool
isExprParam param = do
  params <- ps_exprParams <$> getState
  return (param `S.member` params)

--------------------------------------------------------------------------------
-- Error Messages --------------------------------------------------------------
--------------------------------------------------------------------------------

failWithError :: Loc -> ParseError -> Q a
failWithError loc err = (fail =<<) $ runIO $ do
  src <- readFile $ loc_filename loc
  let line   = lines src !! (sourceLine pos - 1)
  let (x:xs) = lines (show err') ++ [line, caret]
  return $ unlines (x : map ("     " ++) xs)
  where
    err'  = shiftErrPos loc err
    pos   = errorPos err'
    caret = replicate (sourceColumn pos - 1) ' ' ++ "^"

shiftErrPos :: Loc -> ParseError -> ParseError
shiftErrPos loc err = setErrorPos errPos' err
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
  lift $ failWithError loc $ newErrorMessage (Message err) pos

--------------------------------------------------------------------------------
-- Parsing Utility Functions ---------------------------------------------------
--------------------------------------------------------------------------------

named :: String -> Parser a -> Parser a
named s p = p <?> s


located :: Parser a -> Parser (Located a)
located p = do
  s <- getPosition
  a <- p
  e <- getPosition -- TODO: End position minus whitespace!
  return $ mkLocated s e a

withPos :: Parser a -> Parser (SourcePos, a)
withPos p = (,) <$> getPosition <*> p


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

operator :: Parser String
operator = T.operator haskell


whiteSpace :: Parser ()
whiteSpace = T.whiteSpace haskell

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
-- Identifier Parsers ----------------------------------------------------------
--------------------------------------------------------------------------------

varidP, conidP :: Parser String
varidP = named "variable identifier" $ T.identifier $ T.makeTokenParser $ haskellDef
  { T.identStart = lower <|> char '_' }
conidP = named "constructor identifier" $ T.identifier $ T.makeTokenParser $ haskellDef
  { T.identStart = upper }

binderP :: Parser String
binderP = named "binder" $ T.identifier $ T.makeTokenParser $ haskellDef
  { T.identStart = lower <|> char '_'
  , T.reservedNames = T.reservedNames haskellDef ++ ["true", "false", "not", "mod"]
  }


data TyCon = TyCon { tc_op :: Bool
                   , tc_id :: String
                   }

tyConP :: Parser TyCon
tyConP = tyConP' "" <?> "type constructor"

tyConP' :: String -> Parser TyCon
tyConP' prefix = tyConOpP prefix <|> tyConIdP prefix

tyConOpP :: String -> Parser TyCon
tyConOpP prefix = do
  p  <- getPosition
  op <- operator
  return $ TyCon True $ prefix ++ op

tyConIdP :: String -> Parser TyCon
tyConIdP prefix = do
  ident <- unspacedIdent $ haskellDef { T.identStart = upper }
  (char '.' *> tyConP' (prefix ++ ident ++ ".")) <|> (TyCon False (prefix ++ ident) <$ whiteSpace)


tyVarP :: Parser String
tyVarP = varidP <?> "type variable"

exprParamP :: Parser String
exprParamP = conidP <?> "expression parameter"


fTyConP :: Parser FTycon
fTyConP = (FTcInt  <$ reserved "int")
      <|> (FTcInt  <$ reserved "Integer")
      <|> (FTcInt  <$ reserved "Int")
      <|> (FTcInt  <$ reserved "int")
      <|> (FTcReal <$ reserved "real")
      <|> (FTcBool <$ reserved "bool")
      <|> (FTcUser <$> conidP)

