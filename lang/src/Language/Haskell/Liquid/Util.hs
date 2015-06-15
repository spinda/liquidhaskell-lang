module Language.Haskell.Liquid.Util (
    -- * TH Type Utilities
    quantifyTy
  ) where

import Language.Haskell.TH.Syntax

--------------------------------------------------------------------------------
-- TH Type Utilities -----------------------------------------------------------
--------------------------------------------------------------------------------

quantifyTy :: [Name] -> Type -> Type
quantifyTy tvs = ForallT (map PlainTV tvs) []

