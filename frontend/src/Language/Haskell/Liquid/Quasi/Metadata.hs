{-# LANGUAGE DeriveDataTypeable #-}

module Language.Haskell.Liquid.Quasi.Metadata (
    LiquidHaskell
  , LqLocal(..)
  ) where

import           Data.Data
import           Data.Typeable

import           Language.Haskell.Liquid.RType

--------------------------------------------------------------------------------
-- Internal Types for Attaching Metadata --------------------------------------
--------------------------------------------------------------------------------

data LiquidHaskell

data LqLocal =
  LqLocal
    { lql_id :: Int
    , lql_ty :: AnnType
    }
  deriving (Data, Typeable)

