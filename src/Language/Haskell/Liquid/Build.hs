{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

-- TODO: Rename to `Language.Haskell.Liquid.Parse.Build`?

module Language.Haskell.Liquid.Build (
    -- * AST Newtypes
    Span
  , Reft
  , Pred
  , Expr

    -- * Type AST Utility Functions
  , funT
  , appT

    -- * Type-Level Annotations 
  , bind
  , refine
  , exprArgs
  , mkSpan

    -- * Type Declaration Annotations
  , annExprParams
  , annEmbedAs
  , annIsInline

    -- * Reft
  , rPred

    -- * Pred
  , pTrue, pFalse
  , pAnd, pOr, pNot
  , pImp, pIff
  , pExp
  , pAtom
  , pTop

    -- * Expr
  , eConNat
  , eVar
  , eParam
  , eCtr
  , eNeg
  , eBin
  , eIte
  , eBot
  ) where

import Control.Arrow

import Data.List

import Text.Parsec.Pos

import Language.Haskell.TH.Syntax hiding (Pred)
import Language.Haskell.TH.Quote

import Language.Haskell.Liquid.RType hiding (Expr, Pred, Span)
import qualified Language.Haskell.Liquid.RType as RT

--------------------------------------------------------------------------------
-- AST Newtypes ----------------------------------------------------------
--------------------------------------------------------------------------------

newtype Span = S Type
newtype Reft = R Type
newtype Pred = P Type
newtype Expr = E Type

unSpan :: Span -> Type
unSpan (S s) = s
{-# INLINE unSpan #-}

unReft :: Reft -> Type
unReft (R r) = r
{-# INLINE unReft #-}

unPred :: Pred -> Type
unPred (P p) = p
{-# INLINE unPred #-}

unExpr :: Expr -> Type
unExpr (E e) = e
{-# INLINE unExpr #-}

--------------------------------------------------------------------------------
-- Type AST Utility Functions --------------------------------------------------
--------------------------------------------------------------------------------

funT :: Type -> Type -> Type
funT t = (ArrowT `AppT` t `AppT`)

appT :: Type -> [Type] -> Type
appT t = foldl' AppT t

listT :: [Type] -> Type
listT = foldr consT PromotedNilT

tupT :: (Type, Type) -> Type
tupT (x, y) = PromotedTupleT 2 `AppT` x `AppT` y

consT :: Type -> Type -> Type
consT x y = PromotedConsT `AppT` x `AppT` y


toSymbol :: String -> Type
toSymbol = LitT . StrTyLit

toNat :: Int -> Type
toNat = LitT . NumTyLit . fromIntegral

--------------------------------------------------------------------------------
-- Type-Level Annotations ------------------------------------------------------
--------------------------------------------------------------------------------

bind :: Span -> String -> Type -> Type
bind span x a = ConT ''Bind `AppT` unSpan span `AppT` toSymbol x `AppT` a

refine :: String -> Reft -> Type -> Type
refine b r a = ConT ''Refine `AppT` a `AppT` toSymbol b `AppT` unReft r


exprArgs :: [(Span, Expr)] -> Type -> Type
exprArgs es a = ConT ''ExprArgs `AppT` a `AppT` listT (map (tupT . (unSpan *** unExpr)) es)


mkSpan :: SourcePos -> SourcePos -> Span
mkSpan start end = S $ PromotedT 'RT.Span
  `AppT` filename
  `AppT` startLine
  `AppT` startCol
  `AppT` endLine
  `AppT` endCol
  where
    filename  = toSymbol $ sourceName   start
    startLine = toNat    $ sourceLine   start
    startCol  = toNat    $ sourceColumn start
    endLine   = toNat    $ sourceLine   end
    endCol    = toNat    $ sourceColumn end

--------------------------------------------------------------------------------
-- Type Declaration Annotations ------------------------------------------------
--------------------------------------------------------------------------------

annExprParams :: Name -> [String] -> Dec
annExprParams con evs =
  PragmaD $ AnnP (TypeAnnotation con) (SigE expr (ConT ''ExprParams))
  where
    expr = ConE 'ExprParams `AppE` ListE (map (LitE . StringL) evs)

annEmbedAs :: Name -> FTycon -> Dec
annEmbedAs tc fc =
  PragmaD $ AnnP (TypeAnnotation tc) (SigE expr (ConT ''EmbedAs))
  where
    expr                 = ConE 'EmbedAs `AppE` ofFTycon fc
    ofFTycon FTcInt      = ConE 'FTcInt
    ofFTycon FTcReal     = ConE 'FTcReal
    ofFTycon FTcBool     = ConE 'FTcBool
    ofFTycon (FTcUser s) = ConE 'FTcUser `AppE` LitE (StringL s)

annIsInline :: Name -> Dec
annIsInline var =
  PragmaD $ AnnP (ValueAnnotation var) (SigE (ConE 'IsInline) (ConT ''IsInline))

--------------------------------------------------------------------------------
-- Reft ------------------------------------------------------------------------
--------------------------------------------------------------------------------

rPred :: Pred -> Reft
rPred = R . unPred

--------------------------------------------------------------------------------
-- Pred ------------------------------------------------------------------------
--------------------------------------------------------------------------------

pTrue :: Pred
pTrue = P $ PromotedT 'PTrue

pFalse :: Pred
pFalse = P $ PromotedT 'PFalse

pAnd :: Pred -> Pred -> Pred
pAnd (P p1) (P p2) = P $ PromotedT 'PAnd `AppT` p1 `AppT` p2

pOr :: Pred -> Pred -> Pred
pOr (P p1) (P p2) = P $ PromotedT 'POr `AppT` p1 `AppT` p2

pNot :: Pred -> Pred
pNot (P p) = P $ PromotedT 'PNot `AppT` p

pImp :: Pred -> Pred -> Pred
pImp (P p1) (P p2) = P $ PromotedT 'PImp `AppT` p1 `AppT` p2

pIff :: Pred -> Pred -> Pred
pIff (P p1) (P p2) = P $ PromotedT 'PImp `AppT` p1 `AppT` p2

pExp :: Expr -> Pred
pExp (E e) = P $ PromotedT 'PExp `AppT` e

pAtom :: Brel -> Expr -> Expr -> Pred
pAtom op (E e1) (E e2) = P $ PromotedT 'PAtom `AppT` brel op `AppT` e1 `AppT` e2

pTop :: Pred
pTop = P $ PromotedT 'PTop

--------------------------------------------------------------------------------
-- Expr ------------------------------------------------------------------------
--------------------------------------------------------------------------------

eConNat :: Integer -> Expr
eConNat = E . (PromotedT 'ECon `AppT`) . cNat

eVar :: String -> Expr
eVar = E . (PromotedT 'EVar `AppT`) . toSymbol

eParam :: String -> Expr
eParam = E . (PromotedT 'EParam `AppT`) . toSymbol

eCtr :: Span -> String -> Expr
eCtr span = E . (PromotedT 'ECtr `AppT` unSpan span `AppT`) . ConT . mkName

eNeg :: Expr -> Expr
eNeg (E e) = E $ PromotedT 'ENeg `AppT` e

eBin :: Bop -> Expr -> Expr -> Expr
eBin op (E e1) (E e2) = E $ PromotedT 'EBin `AppT` bop op `AppT` e1 `AppT` e2

eIte :: Pred -> Expr -> Expr -> Expr
eIte (P p) (E e1) (E e2) = E $ PromotedT 'EIte `AppT` p `AppT` e1 `AppT` e2

eBot :: Expr
eBot = E $ PromotedT 'EBot


cNat :: Integer -> Type
cNat = (PromotedT 'I `AppT`) . LitT . NumTyLit

brel :: Brel -> Type
brel Eq  = PromotedT 'Eq
brel Ne  = PromotedT 'Ne
brel Gt  = PromotedT 'Gt
brel Ge  = PromotedT 'Ge
brel Lt  = PromotedT 'Lt
brel Le  = PromotedT 'Le
brel Ueq = PromotedT 'Ueq
brel Une = PromotedT 'Une

bop :: Bop -> Type
bop Plus  = PromotedT 'Plus
bop Minus = PromotedT 'Minus
bop Times = PromotedT 'Times
bop Div   = PromotedT 'Div
bop Mod   = PromotedT 'Mod

