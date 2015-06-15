{-# LANGUAGE DeriveDataTypeable #-}

instance Out Name where
  doc = docPrec 0
  docPrec n = text . showSDocUnsafe . pprPrec (fromIntegral n)

instance Out TyCon where
  doc = docPrec 0
  docPrec n = text . showSDocUnsafe . pprPrec (fromIntegral n)

instance Out Var where
  doc = docPrec 0
  docPrec n = text . showSDocUnsafe . pprPrec (fromIntegral n)
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE StandaloneDeriving #-}

module Language.Haskell.Liquid.Types (
    RType(..)
  , RReft
  , SpecType
  ) where

import GHC

import Outputable (pprPrec, showSDocUnsafe)

import Data.Data
import Data.List
import Data.Typeable

import GHC.Generics

import Text.PrettyPrint.GenericPretty
import Text.PrettyPrint.HughesPJ

import Language.Fixpoint.Types

--------------------------------------------------------------------------------
-- Refined Haskell Types -------------------------------------------------------
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

  | RFun       { rt_bind :: !String
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


type SpecType = RType TyCon TyVar RReft

--------------------------------------------------------------------------------
-- Refinement Types ------------------------------------------------------------
--------------------------------------------------------------------------------

type RReft = Reft

--------------------------------------------------------------------------------
-- Skeleton PrettyPrinting -----------------------------------------------------
--------------------------------------------------------------------------------

instance (Out tc, Out tv, Out r) => Out (RType tc tv r)

instance Out a => Out (Located a) where
  doc = docPrec 0
  docPrec n = docPrec n . val

instance Out Constant where
  doc = docPrec 0
  docPrec _ = text . show

instance Out SymConst where
  doc = docPrec 0
  docPrec _ = text . show

instance Out Name where
  doc = docPrec 0
  docPrec n = text . showSDocUnsafe . pprPrec (fromIntegral n)

instance Out TyCon where
  doc = docPrec 0
  docPrec n = text . showSDocUnsafe . pprPrec (fromIntegral n)

instance Out Var where
  doc = docPrec 0
  docPrec n = text . showSDocUnsafe . pprPrec (fromIntegral n)

instance Out Reft
instance Out Refa
instance Out Pred
instance Out Expr

instance Out Brel
instance Out Bop

instance Out Subst
instance Out KVar
instance Out Sort
instance Out FTycon

