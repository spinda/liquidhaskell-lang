module SpecType (
    appRTy
  , appRTys
  , tcAppRTy
  , liftToRTy
  , lowerRTy
  ) where

import TyCon
import TypeRep

import Data.List
import Data.Monoid

import Fixpoint
import RType

--------------------------------------------------------------------------------

appRTy :: SpecType -> SpecType -> RReft -> SpecType
appRTy (RApp tc as r) t2 r' = tcAppRTy tc (as ++ [t2]) (mappend r r')
appRTy t1             t2 r  = RAppTy t1 t2 r

-- TODO: fmap (flip mappend r') <=> strengthen r'
appRTys :: SpecType -> [SpecType] -> RReft -> SpecType
appRTys t              [] r' = fmap (flip mappend r') t 
appRTys (RApp tc as r) ts r' = tcAppRTy tc (as ++ ts) (mappend r r')
appRTys t              ts r' = fmap (flip mappend r') $ foldl' (\t1 t2 -> RAppTy t1 t2 mempty) t ts

tcAppRTy :: TyCon -> [SpecType] -> RReft -> SpecType
tcAppRTy tc as r
  | isFunTyCon tc, [t1, t2] <- as =
    RFun Nothing t1 t2 r
  | otherwise =
    RApp tc as r

liftToRTy :: Type -> SpecType
liftToRTy (TyVarTy tv) = 
  RVar tv mempty
liftToRTy (AppTy t1 t2) =
  RAppTy (liftToRTy t1) (liftToRTy t2) mempty
liftToRTy (TyConApp tc as) =
  RApp tc (map liftToRTy as) mempty
liftToRTy (FunTy i o) =
  RFun Nothing (liftToRTy i) (liftToRTy o) mempty
liftToRTy (ForAllTy tv ty) =
  RAllT tv $ liftToRTy ty
liftToRTy (LitTy _) =
  error "TODO: LiquidHaskell doesn't support type-level literals, yet"

lowerRTy :: SpecType -> Type
lowerRTy (RVar tv _) = 
  TyVarTy tv
lowerRTy (RAppTy t1 t2 _) =
  AppTy (lowerRTy t1) (lowerRTy t2)
lowerRTy (RApp tc as _) =
  TyConApp tc (map lowerRTy as)
lowerRTy (RFun _ i o _) =
  FunTy (lowerRTy i) (lowerRTy o)
lowerRTy (RAllT tv ty) =
  ForAllTy tv $ lowerRTy ty

