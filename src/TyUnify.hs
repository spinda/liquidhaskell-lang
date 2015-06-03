module TyUnify (unifyType) where

import TypeRep

import RType

unifyType :: Type -> AnnType -> SpecType
unifyType (TyVarTy tv) (RVar _ r) = 
  RVar tv r
unifyType (AppTy t1 t2) (RAppTy t1' t2' r) =
  RAppTy (unifyType t1 t1') (unifyType t2 t2') r
unifyType (TyConApp tc as) (RApp _ as' r) =
  RApp tc (zipWith unifyType as as') r
unifyType (FunTy i o) (RFun b i' o' r) =
  RFun b (unifyType i i') (unifyType o o') r
unifyType (ForAllTy tv ty) (RAllT _ ty') =
  RAllT tv (unifyType ty ty')
unifyType (LitTy _) _ =
  error "TODO: LiquidHaskell doesn't support type-level literals, yet"

