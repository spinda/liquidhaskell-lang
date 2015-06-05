module TyUnify (unifyRTy) where

import TypeRep

import RType

unifyRTy :: Type -> AnnType -> SpecType
unifyRTy (TyVarTy tv) (RVar () r) =
  RVar tv r
unifyRTy (AppTy t1 t2) (RAppTy t1' t2' r) =
  RAppTy (unifyRTy t1 t1') (unifyRTy t2 t2') r
unifyRTy (TyConApp tc as) (RApp () as' r) =
  RApp tc (zipWith unifyRTy as as') r
unifyRTy (FunTy i o) (RFun b i' o' r) =
  RFun b (unifyRTy i i') (unifyRTy o o') r
unifyRTy (ForAllTy tv ty) (RAllT () ty') =
  RAllT tv (unifyRTy ty ty')
unifyRTy (LitTy _) _ =
  error "TODO: LiquidHaskell doesn't support type-level literals, yet"

