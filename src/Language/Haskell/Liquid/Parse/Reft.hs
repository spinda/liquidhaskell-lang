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
ptermP = expOrAtomP
     <|> parens predP
     <|> (pTrue  <$ reserved "true")
     <|> (pFalse <$ reserved "false")

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
etermP = do
  es <- many1 (located etermP')
  return $ case es of
    [e]     -> val e
    (e:es') -> eApp e (val <$> es')

etermP' :: Parser Expr
etermP' = parens exprP
      <|> (eBot <$ reservedOp "_|_")
      <|> eIteP
      <|> (eConNat <$> natural)
      <|> (eVar <$> binderP)
      <|> eCtrP

eCtrP :: Parser Expr
eCtrP = do
  c <- (located conidP) <?> "data constructor or expression parameter"
  genExprParam <- isExprParam (val c)
  return $ if genExprParam
    then eParam (val c)
    else eCtr c

eIteP :: Parser Expr
eIteP = eIte <$> (reservedOp "if"   *> predP)
             <*> (reservedOp "then" *> exprP)
             <*> (reservedOp "else" *> exprP)

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

