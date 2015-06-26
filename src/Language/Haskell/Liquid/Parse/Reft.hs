module Language.Haskell.Liquid.Parse.Reft (
    reftP
  , predP
  , exprP
  ) where

import Language.Haskell.Liquid.Build
import Language.Haskell.Liquid.RType (Brel(..), Bop(..))

import Language.Haskell.Liquid.Parse.Base

import Text.Parsec hiding (Pred)
import Text.Parsec.Combinator
import Text.Parsec.Expr

--------------------------------------------------------------------------------
-- Reft ------------------------------------------------------------------------
--------------------------------------------------------------------------------

reftP :: Parser Reft
reftP = rPred <$> predP

--------------------------------------------------------------------------------
-- Pred ------------------------------------------------------------------------
--------------------------------------------------------------------------------

predP :: Parser Pred
predP = buildExpressionParser pops ptermP <?> "predicate"

ptermP :: Parser Pred
ptermP = parens predP
    <|> (pTrue  <$ reserved "true")
    <|> (pFalse <$ reserved "false")
    <|> expOrAtomP

expOrAtomP :: Parser Pred
expOrAtomP = do
  e1 <- exprP
  option (pExp e1) $ do
    op <- brelP
    e2 <- exprP
    return $ pAtom op e1 e2

pops = [ [Prefix (pNot <$ reservedOp "~"  )           ]
       , [Prefix (pNot <$ reserved   "not")           ]
       , [Infix  (pAnd <$ reservedOp "&&" ) AssocRight]
       , [Infix  (pOr  <$ reservedOp "||" ) AssocRight]
       , [Infix  (pImp <$ reservedOp "=>" ) AssocRight]
       , [Infix  (pImp <$ reservedOp "==>") AssocRight]
       , [Infix  (pIff <$ reservedOp "<=>") AssocRight]
       ]

brelP :: Parser Brel
brelP = Eq  <$ reservedOp "=="
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

exprP :: Parser Expr
exprP = buildExpressionParser eops etermP <?> "expression"

etermP :: Parser Expr
etermP = parens exprP
    <|> (eBot <$ reservedOp "_|_")
    <|> eIteP
    <|> (eConNat <$> natural)
    <|> (eBdr <$> binderP)
    <|> eCtrP

eIteP :: Parser Expr
eIteP = eIte <$> (reservedOp "if"   *> predP)
             <*> (reservedOp "then" *> exprP)
             <*> (reservedOp "else" *> exprP)

eCtrP :: Parser Expr
eCtrP = do
  v <- conidP <?> "data constructor or expression parameter"
  genExprParam <- isExprParam v
  return $ if genExprParam
    then eParam v
    else eBdr v

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

