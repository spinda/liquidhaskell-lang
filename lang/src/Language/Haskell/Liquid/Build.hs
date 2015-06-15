{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Haskell.Liquid.Build (
    -- * AST-Building Types
    Located(..)

  , Reft, unReft
  , Pred, unPred
  , Expr, unExpr

    -- * Type AST Utility Functions
  , funT

    -- * Type-Level Annotations 
  , bind
  , refine

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
  , eBdr
  , eCtr
  , eNeg
  , eBin
  , eIte
  , eBot
  ) where

import Text.Parsec.Pos

import Language.Haskell.TH.Syntax hiding (Pred)

import Language.Haskell.Liquid.RType hiding (Pred, Expr)

--------------------------------------------------------------------------------
-- AST-Building Types ----------------------------------------------------------
--------------------------------------------------------------------------------

data Located a =
  Located
    { l_start :: SourcePos
    , l_value :: a
    , l_end   :: SourcePos
    }
  deriving (Functor)


newtype Reft = Reft Type
newtype Pred = Pred Type
newtype Expr = Expr Type

unReft :: Reft -> Type
unReft (Reft r) = r
{-# INLINE unReft #-}

unPred :: Pred -> Type
unPred (Pred p) = p
{-# INLINE unPred #-}

unExpr :: Expr -> Type
unExpr (Expr e) = e
{-# INLINE unExpr #-}

--------------------------------------------------------------------------------
-- Type AST Utility Functions --------------------------------------------------
--------------------------------------------------------------------------------

funT :: Type -> Type -> Type
funT t = (ArrowT `AppT` t `AppT`)

toSymbol :: String -> Type
toSymbol = LitT . StrTyLit

toNat :: Int -> Type
toNat = LitT . NumTyLit . fromIntegral

--------------------------------------------------------------------------------
-- Type-Level Annotations ------------------------------------------------------
--------------------------------------------------------------------------------

bind :: Located String -> Type -> Type
bind x a = ConT ''Bind `AppT` located (toSymbol <$> x) `AppT` a

refine :: Type -> Reft -> Type
refine a r = ConT ''Refine `AppT` a `AppT` unReft r


located :: Located Type -> Type
located (Located start t end) =
  PromotedT 'L `AppT` filename
               `AppT` startLine
               `AppT` startCol
               `AppT` endLine
               `AppT` endCol
               `AppT` t
  where
    filename  = toSymbol $ sourceName   start
    startLine = toNat    $ sourceLine   start
    startCol  = toNat    $ sourceColumn start
    endLine   = toNat    $ sourceLine   end
    endCol    = toNat    $ sourceColumn end

--------------------------------------------------------------------------------
-- Reft ------------------------------------------------------------------------
--------------------------------------------------------------------------------

rPred :: Located Pred -> Reft
rPred = Reft . located . fmap unPred

--------------------------------------------------------------------------------
-- Pred ------------------------------------------------------------------------
--------------------------------------------------------------------------------

pTrue :: Pred
pTrue = Pred $ PromotedT 'PTrue

pFalse :: Pred
pFalse = Pred $ PromotedT 'PFalse

pAnd :: Pred -> Pred -> Pred
pAnd (Pred p1) (Pred p2) = Pred $ PromotedT 'PAnd `AppT` p1 `AppT` p2

pOr :: Pred -> Pred -> Pred
pOr (Pred p1) (Pred p2) = Pred $ PromotedT 'POr `AppT` p1 `AppT` p2

pNot :: Pred -> Pred
pNot (Pred p) = Pred $ PromotedT 'PNot `AppT` p

pImp :: Pred -> Pred -> Pred
pImp (Pred p1) (Pred p2) = Pred $ PromotedT 'PImp `AppT` p1 `AppT` p2

pIff :: Pred -> Pred -> Pred
pIff (Pred p1) (Pred p2) = Pred $ PromotedT 'PImp `AppT` p1 `AppT` p2

pExp :: Expr -> Pred
pExp (Expr e) = Pred $ PromotedT 'PExp `AppT` e

pAtom :: Brel -> Expr -> Expr -> Pred
pAtom op (Expr e1) (Expr e2) = Pred $ PromotedT 'PAtom `AppT` brel op `AppT` e1 `AppT` e2

pTop :: Pred
pTop = Pred $ PromotedT 'PTop

--------------------------------------------------------------------------------
-- Expr ------------------------------------------------------------------------
--------------------------------------------------------------------------------

eConNat :: Integer -> Expr
eConNat = Expr . (PromotedT 'ECon `AppT`) . cNat

eBdr :: Located String -> Expr
eBdr = Expr . (PromotedT 'EBdr `AppT`) . located . fmap toSymbol

eCtr :: Located String -> Expr
eCtr = Expr . (PromotedT 'ECtr `AppT`) . located . fmap (PromotedT . mkName)

eNeg :: Expr -> Expr
eNeg (Expr e) = Expr $ PromotedT 'ENeg `AppT` e

eBin :: Bop -> Expr -> Expr -> Expr
eBin op (Expr e1) (Expr e2) = Expr $ PromotedT 'EBin `AppT` bop op `AppT` e1 `AppT` e2

eIte :: Pred -> Expr -> Expr -> Expr
eIte (Pred p) (Expr e1) (Expr e2) = Expr $ PromotedT 'EIte `AppT` p `AppT` e1 `AppT` e2

eBot :: Expr
eBot = Expr $ PromotedT 'EBot


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

