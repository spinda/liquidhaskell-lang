{-# LANGUAGE TupleSections #-}

module Language.Haskell.Liquid.Extract (
    extractTopSigs
--  , extractLocalSigs
--  , extractTySyns
  ) where

import GHC

import MonadUtils
import Var

import Language.Haskell.Liquid.Reify
import Language.Haskell.Liquid.Types
import Language.Haskell.Liquid.WiredIns

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

extractTopSigs :: GhcMonad m => TypecheckedModule -> WiredM m [(Var, SpecType)]
extractTopSigs = mapMaybeM go . modInfoTyThings . tm_checked_module_info
  where
    go (AnId id) = (Just . (id, )) <$> reifyRTy (idType id)
    go _         = return Nothing

--extractLocalSigs :: GhcMonad m => TypecheckedModule -> WiredM m [(Var, SpecType)]
--extractLocalSigs

