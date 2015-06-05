{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

module RType where

import GHC (TyCon, TyVar)
import qualified Name as N
import Outputable (pprPrec, showSDocUnsafe)

import Data.Data hiding (TyCon)
import Data.Typeable hiding (TyCon)

import GHC.Generics

import Language.Haskell.TH.Syntax hiding (Pred)

import Text.PrettyPrint.GenericPretty
import Text.PrettyPrint.HughesPJ

import Fixpoint

data RType tc tv r
  = RVar       { rt_var    :: !tv
               , rt_reft   :: !r
               }

  | RAppTy     { rt_arg    :: !(RType tc tv r)
               , rt_res    :: !(RType tc tv r)
               , rt_reft   :: !r
               }

  | RApp       { rt_tycon  :: !tc
               , rt_args   :: ![RType tc tv r]
               , rt_reft   :: !r
               }

  | RFun       { rt_bind   :: !(Maybe String)
               , rt_in     :: !(RType tc tv r)
               , rt_out    :: !(RType tc tv r)
               , rt_reft   :: !r
               }

  | RAllT      { rt_tvbind :: !tv
               , rt_ty     :: !(RType tc tv r)
               }

  deriving (Eq, Show, Data, Typeable, Generic)

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

type QuasiType = RType Name  Name  RReft
type AnnType   = RType ()    ()    RReft
type SpecType  = RType TyCon TyVar RReft

type RReft = Reft

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

instance Out Type
instance Out TyVarBndr
instance Out TyLit

instance Out Reft where
instance Out Refa where
instance Out Pred where
instance Out Expr where

{-
instance Out Reft where
  doc = docPrec 0
  docPrec _ = text . showFix

instance Out Refa where
  doc = docPrec 0
  docPrec _ = text . showFix

instance Out Pred where
  doc = docPrec 0
  docPrec _ = text . showFix

instance Out Expr where
  doc = docPrec 0
  docPrec _ = text . showFix
-}

instance Out Brel
instance Out Bop

instance Out Subst
instance Out KVar
instance Out Sort
instance Out FTycon

instance Out N.Name where
  doc = docPrec 0
  docPrec n = text . showSDocUnsafe . pprPrec (fromIntegral n)

instance Out TyCon where
  doc = docPrec 0
  docPrec n = text . showSDocUnsafe . pprPrec (fromIntegral n)

instance Out TyVar where
  doc = docPrec 0
  docPrec n = text . showSDocUnsafe . pprPrec (fromIntegral n)

skel :: Out a => a -> String
skel = render . doc

