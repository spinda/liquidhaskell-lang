{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Syn (LiquidHaskell, LqLocal(..)) where

import Data.Data
import Data.Typeable

import RType

data LiquidHaskell

data LqLocal =
  LqLocal
    { lql_id :: Int
    , lql_ty :: AnnType
    }
  deriving (Data, Typeable)

