{-# LANGUAGE TupleSections #-}

module Expand (expandRTys) where

import GHC

import Name
import NameEnv
import TyCon
import Type

import Control.Monad.State
import Data.Maybe

import Fixpoint
import RType
import SpecType
import Subst

--------------------------------------------------------------------------------

expandRTys :: [(Name, SpecType)] -> [(Name, SpecType)]
expandRTys ts = evalState (mapM (expandRTy.fst) ts) initState
  where
    initState = mkNameEnv $ map (\(name, ty) -> (name, (False, ty))) ts

--------------------------------------------------------------------------------

type Expand = State (NameEnv (Bool, SpecType))

expandRTy :: Name -> Expand (Name, SpecType)
expandRTy name = do
  Just (expanded, ty) <- gets (\env -> lookupNameEnv env name)
  if expanded
     then return (name, ty)
     else (name, ) <$> expandRTy' name ty

expandRTy' :: Name -> SpecType -> Expand SpecType
expandRTy' name ty = do
  ty' <- go ty
  modify (\env -> extendNameEnv env name (True, ty'))
  return ty'
  where
    go (RApp tc as r) = do
      as' <- mapM go as
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

expandTySyn :: Name -> Type -> Expand SpecType
expandTySyn name rhs = do
  env <- get
  case lookupNameEnv env name of
    Nothing ->
      expandRTy' name $ liftToRTy rhs
    Just (False, ty) ->
      expandRTy' name ty
    Just (True, ty) ->
      return ty

--------------------------------------------------------------------------------

appSyn :: TyCon -> [SpecType] -> Maybe (RTySubst, [SpecType], Type)
appSyn tc as
  | Just (tvs, rhs) <- synTyConDefn_maybe tc, length tvs <= length as =
    Just (mkRTySubst $ zip tvs as, drop (length tvs) as, rhs)
  | otherwise =
    Nothing

