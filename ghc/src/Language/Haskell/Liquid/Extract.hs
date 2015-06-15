module Language.Haskell.Liquid.Extract (
    extractTopSigs
  , extractLocalSigs
--  , extractTySyns
  ) where

import GHC

import Language.Haskell.Liquid.Reify
import Language.Haskell.Liquid.Types
import Language.Haskell.Liquid.WiredIns

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

extractTopSigs :: GhcModule m => TypecheckedModule -> WiredM m [(Var, SpecType)]
extractTopSigs

extractLocalSigs :: GhcModule m => TypecheckedModule -> WiredM m [(Var, SpecType)]
extractLocalSigs

