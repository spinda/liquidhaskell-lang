{-# LANGUAGE TupleSections #-}

module Language.Haskell.Liquid.Expand (
    expandRTys
  ) where

import           GHC

import           Name
import           NameEnv
import           TyCon
import           Type

import           Control.Arrow
import           Control.Monad.State

import           Language.Haskell.Liquid.RType
import           Language.Haskell.Liquid.SpecType
import           Language.Haskell.Liquid.Subst

--------------------------------------------------------------------------------
-- Top-Level Entry Point -------------------------------------------------------
--------------------------------------------------------------------------------

expandRTys :: [(Name, SpecType)] -> [(Name, SpecType)]
expandRTys ts = evalState (mapM (expandRTy.fst) ts) initState
  where
    initState = mkNameEnv $ map (second Unexpanded) ts


type ExpandM = State (NameEnv ExpType)

data ExpType = Expanded   { et_type :: SpecType }
             | Unexpanded { et_type :: SpecType }

--------------------------------------------------------------------------------
-- Internal Expansion Functions ------------------------------------------------
--------------------------------------------------------------------------------

expandRTy :: Name -> ExpandM (Name, SpecType)
expandRTy name = do
  env <- get
  case lookupNameEnv env name of
    Just (Expanded ty) ->
      return (name, ty)
    Just (Unexpanded ty) ->
      (name, ) <$> expandRTy' name ty

expandRTy' :: Name -> SpecType -> ExpandM SpecType
expandRTy' name ty = do
  ty' <- go ty
  modify $ \env -> extendNameEnv env name $ Expanded ty'
  return ty'
  where
    go (RApp tc as r) = do
      as'<- mapM go as
      case appSyn tc as' of
        Just (ss, ras, rhs) -> do
          rhs' <- expandTySyn (getName tc) rhs
          go $ appRTys (substRTy ss rhs') ras r
        Nothing ->
          return $ RApp tc as' r
    go (RVar tv r) =
      return $ RVar tv r
    go (RAppTy t1 t2 r) =
      appRTy <$> go t1 <*> go t2 <*> pure r
    go (RFun b i o r) =
      RFun b <$> go i <*> go o <*> pure r
    go (RAllT tv ty) =
      RAllT tv <$> go ty

expandTySyn :: Name -> Type -> ExpandM SpecType
expandTySyn name rhs = do
  env <- get
  case lookupNameEnv env name of
    Nothing ->
      expandRTy' name $ ofTypeRTy rhs
    Just (Unexpanded ty) ->
      expandRTy' name ty
    Just (Expanded ty) ->
      return ty


appSyn :: TyCon -> [SpecType] -> Maybe (RTySubst, [SpecType], Type)
appSyn tc as
  | Just (tvs, rhs) <- synTyConDefn_maybe tc, length tvs <= length as =
    Just (mkRTySubst $ zip tvs as, drop (length tvs) as, rhs)
  | otherwise =
    Nothing

