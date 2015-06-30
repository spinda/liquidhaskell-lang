{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

-- TODO: Rename to `Language.Haskell.Liquid.Parse.Build`?

module Language.Haskell.Liquid.Build (
    -- * AST Newtypes
    Reft
  , Pred
  , Expr

    -- * Location Information
  , Located
  , val
  , mkLocated

    -- * AST Utility Functions
  , funT

    -- * Type-Level Annotations 
  , bind
  , refine
  , exprArgs

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

import Language.Haskell.Liquid.RType hiding (Expr, Pred)
import qualified Language.Haskell.Liquid.RType as RT

--------------------------------------------------------------------------------
-- AST Newtypes ----------------------------------------------------------------
--------------------------------------------------------------------------------

newtype Reft = R Type
newtype Pred = P Type
newtype Expr = E Type

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
-- Location Information --------------------------------------------------------
--------------------------------------------------------------------------------

val :: Located a -> a
val = loc_value
{-# INLINE val #-}


mkLocated :: SourcePos -> SourcePos -> a -> Located a
mkLocated st ed = Located (mkSpan st ed)

mkSpan :: SourcePos -> SourcePos -> Span
mkSpan st ed = Span (mkPos st) (mkPos ed)

mkPos :: SourcePos -> Pos
mkPos p = Pos (sourceName p) (sourceLine p) (sourceColumn p)


locE :: Located Exp -> Exp
locE (Located span exp) =
  ConE 'Located `AppE` spanE span `AppE` exp

locT :: Located Type -> Type
locT (Located span ty) =
  PromotedT 'TyLocated `AppT` spanT span `AppT` ty


spanE :: Span -> Exp
spanE (Span st ed) =
  ConE 'Span `AppE` posE st `AppE` posE ed

spanT :: Span -> Type
spanT (Span st ed) =
  PromotedT 'TySpan `AppT` posT st `AppT` posT ed


posE :: Pos -> Exp
posE (Pos name line col) =
  ConE 'Pos `AppE` strE name `AppE` intE line `AppE` intE col

posT :: Pos -> Type
posT (Pos name line col) =
  PromotedT 'TyPos `AppT` strT name `AppT` natT line `AppT` natT col

--------------------------------------------------------------------------------
-- AST Utility Functions -------------------------------------------------------
--------------------------------------------------------------------------------

funT :: Type -> Type -> Type
funT t = (ArrowT `AppT` t `AppT`)

listT :: [Type] -> Type
listT = foldr consT PromotedNilT

tupT :: (Type, Type) -> Type
tupT (x, y) = PromotedTupleT 2 `AppT` x `AppT` y

consT :: Type -> Type -> Type
consT x y = PromotedConsT `AppT` x `AppT` y

strT :: String -> Type
strT = LitT . StrTyLit

natT :: Int -> Type
natT = LitT . NumTyLit . fromIntegral


strE :: String -> Exp
strE = LitE . StringL

intE :: Int -> Exp
intE = LitE . IntegerL . fromIntegral

--------------------------------------------------------------------------------
-- Type-Level Annotations ------------------------------------------------------
--------------------------------------------------------------------------------

bind :: Located String -> Type -> Type
bind x a = ConT ''Bind `AppT` locT (strT <$> x) `AppT` a

refine :: String -> Reft -> Type -> Type
refine b r a = ConT ''Refine `AppT` a `AppT` strT b `AppT` unReft r

exprArgs :: [Located Expr] -> Type -> Type
exprArgs es a = ConT ''ExprArgs `AppT` a `AppT` listT (map (locT . fmap unExpr) es)

--------------------------------------------------------------------------------
-- Type Declaration Annotations ------------------------------------------------
--------------------------------------------------------------------------------

annExprParams :: Name -> Located [String] -> Dec
annExprParams tc =
  PragmaD . AnnP (TypeAnnotation tc) .
    (ConE 'ExprParams `AppE`) . locE . fmap (ListE . map strE)

annEmbedAs :: Name -> Located FTycon -> Dec
annEmbedAs tc =
  PragmaD . AnnP (TypeAnnotation tc) .
    (ConE 'EmbedAs `AppE`) . locE . fmap ofFTycon
  where
    ofFTycon FTcInt      = ConE 'FTcInt
    ofFTycon FTcReal     = ConE 'FTcReal
    ofFTycon FTcBool     = ConE 'FTcBool
    ofFTycon (FTcUser s) = ConE 'FTcUser `AppE` LitE (StringL s)

annIsInline :: Located Name -> Dec
annIsInline var =
  PragmaD $ AnnP (ValueAnnotation $ val var) $
    ConE 'IsInline `AppE` spanE (loc_span var)

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
eVar = E . (PromotedT 'EVar `AppT`) . strT

eParam :: String -> Expr
eParam = E . (PromotedT 'EParam `AppT`) . strT

eCtr :: Located String -> Expr
eCtr = E . (PromotedT 'ECtr `AppT`) . locT . fmap (ConT . mkName)

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

