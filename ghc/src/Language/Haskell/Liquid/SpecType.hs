module Language.Haskell.Liquid.SpecType (
    SpecType,
    appRTy,
    appRTys,
    tcAppRTy,
    strengthenRTy,
    ofTypeRTy,
    toTypeRTy,
    instantiateRTy
  ) where

import           GHC

import           Outputable (pprPrec, showSDocUnsafe)
import           Type
import           TypeRep

import           Data.List
import           Data.Maybe
import           Data.Monoid

import           Language.Haskell.Liquid.RType

import           Text.PrettyPrint.GenericPretty
import           Text.PrettyPrint.HughesPJ

--------------------------------------------------------------------------------
-- GHC-Instantiated Refinement Types -------------------------------------------
--------------------------------------------------------------------------------

type SpecType = RType TyCon TyVar RReft

--------------------------------------------------------------------------------
-- Skeleton PrettyPrinting for GHC Types ---------------------------------------
--------------------------------------------------------------------------------

instance Out Name where
  doc = docPrec 0
  docPrec n = text . showSDocUnsafe . pprPrec (fromIntegral n)

instance Out TyCon where
  doc = docPrec 0
  docPrec n = text . showSDocUnsafe . pprPrec (fromIntegral n)

instance Out Var where
  doc = docPrec 0
  docPrec n = text . showSDocUnsafe . pprPrec (fromIntegral n)

--------------------------------------------------------------------------------
-- Operations on Type Refinements ----------------------------------------------
--------------------------------------------------------------------------------

strengthenRTy :: RReft -> SpecType -> SpecType
strengthenRTy r = fmap (`mappend` r)

--------------------------------------------------------------------------------
-- Refinement Type Application -------------------------------------------------
--------------------------------------------------------------------------------

appRTy :: SpecType -> SpecType -> RReft -> SpecType
appRTy (RApp tc as r) t2 r' = tcAppRTy tc (as ++ [t2]) (mappend r r')
appRTy t1             t2 r  = RAppTy t1 t2 r

appRTys :: SpecType -> [SpecType] -> RReft -> SpecType
appRTys t              [] r' = strengthenRTy r' t
appRTys (RApp tc as r) ts r' = tcAppRTy tc (as ++ ts) (mappend r r')
appRTys t              ts r' = strengthenRTy r' $ foldl' (\t1 t2 -> RAppTy t1 t2 mempty) t ts

tcAppRTy :: TyCon -> [SpecType] -> RReft -> SpecType
tcAppRTy tc as r
  | isFunTyCon tc, [t1, t2] <- as =
    RFun Nothing t1 t2 r
  | otherwise =
    RApp tc as r

--------------------------------------------------------------------------------
-- Type<->SpecType Transforms --------------------------------------------------
--------------------------------------------------------------------------------

ofTypeRTy :: Type -> SpecType
ofTypeRTy (TyVarTy tv) =
  RVar tv mempty
ofTypeRTy (AppTy t1 t2) =
  RAppTy (ofTypeRTy t1) (ofTypeRTy t2) mempty
ofTypeRTy (TyConApp tc as) =
  RApp tc (map ofTypeRTy as) mempty
ofTypeRTy (FunTy i o) =
  RFun Nothing (ofTypeRTy i) (ofTypeRTy o) mempty
ofTypeRTy (ForAllTy tv ty) =
  RAllT tv $ ofTypeRTy ty
ofTypeRTy (LitTy _) =
  error "TODO: LiquidHaskell doesn't support type-level literals, yet"

toTypeRTy :: SpecType -> Type
toTypeRTy (RVar tv _) =
  TyVarTy tv
toTypeRTy (RAppTy t1 t2 _) =
  AppTy (toTypeRTy t1) (toTypeRTy t2)
toTypeRTy (RApp tc as _) =
  TyConApp tc (map toTypeRTy as)
toTypeRTy (RFun _ i o _) =
  FunTy (toTypeRTy i) (toTypeRTy o)
toTypeRTy (RAllT tv ty) =
  ForAllTy tv $ toTypeRTy ty

--------------------------------------------------------------------------------
-- Instantiate Annotation Types with GHC Uniques -------------------------------
--------------------------------------------------------------------------------

instantiateRTy :: Type -> AnnType -> SpecType
instantiateRTy (TyVarTy tv) (RVar () r) =
  RVar tv r
instantiateRTy (AppTy t1 t2) (RAppTy t1' t2' r) =
  RAppTy (instantiateRTy t1 t1') (instantiateRTy t2 t2') r
instantiateRTy (TyConApp tc as) (RApp () as' r) =
  RApp tc (zipWith instantiateRTy as as') r
instantiateRTy (FunTy i o) (RFun b i' o' r) =
  RFun b (instantiateRTy i i') (instantiateRTy o o') r
instantiateRTy (ForAllTy tv ty) (RAllT () ty') =
  RAllT tv (instantiateRTy ty ty')
instantiateRTy (LitTy _) _ =
  error "TODO: LiquidHaskell doesn't support type-level literals, yet"

