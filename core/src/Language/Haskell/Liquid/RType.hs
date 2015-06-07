{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE StandaloneDeriving #-}

module Language.Haskell.Liquid.RType (
    RType(..)
  , QuasiType, AnnType
  , RReft

  , quantifyRTy
  , splitRTy
  , simplRTy
  , stripRTy
  ) where

import           Data.Data
import           Data.List
import           Data.Typeable

import           GHC.Generics

import           Language.Haskell.TH.Syntax       hiding (Pred)

import           Text.PrettyPrint.GenericPretty
import           Text.PrettyPrint.HughesPJ

import           Language.Haskell.Liquid.Fixpoint

--------------------------------------------------------------------------------
-- Refinement Types Core -------------------------------------------------------
--------------------------------------------------------------------------------

data RType tc tv r
  = RVar       { rt_var  :: !tv
               , rt_reft :: !r
               }

  | RAppTy     { rt_arg  :: !(RType tc tv r)
               , rt_res  :: !(RType tc tv r)
               , rt_reft :: !r
               }

  | RApp       { rt_tycon :: !tc
               , rt_args  :: ![RType tc tv r]
               , rt_reft  :: !r
               }

  | RFun       { rt_bind :: !(Maybe String)
               , rt_in   :: !(RType tc tv r)
               , rt_out  :: !(RType tc tv r)
               , rt_reft :: !r
               }

  | RAllT      { rt_tvbind :: !tv
               , rt_ty     :: !(RType tc tv r)
               }

  deriving ( Eq, Show
           , Data, Typeable, Generic
           )

deriving instance Functor (RType tc tv)
deriving instance Foldable (RType tc tv)
deriving instance Traversable (RType tc tv)

type QuasiType = RType Name  Name  RReft
type AnnType   = RType ()    ()    RReft

type RReft = Reft

{-
instance Functor (RType tc tv) where
  fmap f (RVar tv r) =
    RVar tv $ f r
  fmap f (RAppTy t1 t2 r) =
    RAppTy (fmap f t1) (fmap f t2) (f r)
  fmap f (RApp tc as r) =
    RApp tc (map (fmap f) as) (f r)
  fmap f (RFun b i o r) =
    RFun b (fmap f i) (fmap f o) (f r)
  fmap f (RAllT tv ty) =
    RAllT tv $ fmap f ty
-}

--------------------------------------------------------------------------------
-- Skeleton PrettyPrinting -----------------------------------------------------
--------------------------------------------------------------------------------

instance (Out tc, Out tv, Out r) => Out (RType tc tv r)

instance Out Name where
  doc = docPrec 0
  docPrec _ = text . show

instance Out a => Out (Located a) where
  doc = docPrec 0
  docPrec n = docPrec n . val

instance Out Constant where
  doc = docPrec 0
  docPrec _ = text . show

instance Out SymConst where
  doc = docPrec 0
  docPrec _ = text . show

instance Out Reft where
instance Out Refa where
instance Out Pred where
instance Out Expr where

instance Out Brel
instance Out Bop

instance Out Subst
instance Out KVar
instance Out Sort
instance Out FTycon

--------------------------------------------------------------------------------
-- Operations on Refinement Types ----------------------------------------------
--------------------------------------------------------------------------------

quantifyRTy :: [tv] -> RType tc tv r -> RType tc tv r
quantifyRTy tvs ty = foldr RAllT ty $ reverse tvs

splitRTy :: QuasiType -> (Type, AnnType)
splitRTy t = (simplRTy t, stripRTy t)

simplRTy :: QuasiType -> Type
simplRTy (RVar tv _)
  = VarT tv
simplRTy (RAppTy t1 t2 _)
  = AppT (simplRTy t1) (simplRTy t2)
simplRTy (RApp tc as _)
  = foldl' AppT (ConT tc) (map simplRTy as)
simplRTy (RFun _ i o _)
  = ArrowT `AppT` simplRTy i `AppT` simplRTy o
simplRTy (RAllT tv ty)
  = let (tvs, rest) = collect [tv] ty
    in  ForallT (map PlainTV $ reverse tvs) [] (simplRTy rest)
  where
    collect tvs (RAllT tv rest) =
      collect (tv:tvs) rest
    collect tvs rest =
      (tvs, rest)

stripRTy :: QuasiType -> AnnType
stripRTy (RVar _ r)
  = RVar () r
stripRTy (RAppTy t1 t2 r)
  = RAppTy (stripRTy t1) (stripRTy t2) r
stripRTy (RApp _ as r)
  = RApp () (map stripRTy as) r
stripRTy (RFun b i o r)
  = RFun b (stripRTy i) (stripRTy o) r
stripRTy (RAllT _ ty)
  = RAllT () (stripRTy ty)

