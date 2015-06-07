module Language.Haskell.Liquid.Subst (
    substRTy

  , RTySubst
  , mkRTySubst
  ) where

import           GHC                              (TyVar)

import           TcType
import           Var
import           VarEnv
import           VarSet

import           Control.Arrow
import           Data.List
import           Data.Monoid

import           Language.Haskell.Liquid.RType
import           Language.Haskell.Liquid.SpecType

--------------------------------------------------------------------------------
-- Type Substitution Function --------------------------------------------------
--------------------------------------------------------------------------------

substRTy :: RTySubst -> SpecType -> SpecType
substRTy ss ty
  | nullRTySubst ss = ty
  | otherwise       = substRTy' ss ty


substRTy' :: RTySubst -> SpecType -> SpecType

substRTy' ss (RVar tv r)
  | Just ty <- lookupRTySubst ss tv = strengthenRTy r ty
  | otherwise                       = RVar tv r

substRTy' ss (RAppTy t1 t2 r) =
  appRTy (substRTy' ss t1) (substRTy' ss t2) r

substRTy' ss (RApp tc as r) =
  RApp tc (map (substRTy' ss) as) r

substRTy' ss (RFun b i o r) =
  RFun b (substRTy' ss i) (substRTy' ss o) r

substRTy' ss (RAllT tv ty) =
  RAllT tv' $ substRTy' ss' ty
  where
    (ss', tv') = forAllRTySubst ss tv

--------------------------------------------------------------------------------
-- RTySubst Type and Operations ------------------------------------------------
--------------------------------------------------------------------------------

data RTySubst =
  RTS
    { rts_sub :: TvSubst
    , rts_env :: TyVarEnv SpecType
    }


mkRTySubst :: [(TyVar, SpecType)] -> RTySubst
mkRTySubst tvtys =
  RTS (mkTopTvSubst $ map (second toTypeRTy) tvtys)
      (mkVarEnv tvtys)

lookupRTySubst :: RTySubst -> TyVar -> Maybe SpecType
lookupRTySubst ss = lookupVarEnv (rts_env ss)


nullRTySubst :: RTySubst -> Bool
nullRTySubst = isEmptyVarEnv . rts_env

forAllRTySubst :: RTySubst -> TyVar -> (RTySubst, TyVar)
forAllRTySubst (RTS sub@(TvSubst scope tenv) env) tv
  | closedKind && tv' == tv =
    ( RTS (TvSubst scope' $ delVarEnv tenv tv)
          (delVarEnv env tv)
    , tv'
    )
  | otherwise =
    ( RTS (TvSubst scope' $ extendVarEnv tenv tv $ mkTyVarTy tv')
          (extendVarEnv env tv $ RVar tv' mempty)
    , tv'
    )
  where
    closedKind = isEmptyVarSet $ tyVarsOfType $ tyVarKind tv
    tv' | closedKind = uniqAway (getTvInScope sub) tv
        | otherwise  = uniqAway (getTvInScope sub) $ updateTyVarKind (substTy sub) tv
    scope' = extendInScopeSet scope tv'

