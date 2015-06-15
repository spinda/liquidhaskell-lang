module Lanugage.Haskell.Liquid.WiredIns (
    WiredM
  , runWiredM

  , WiredIns(..)
  , loadWiredIns
  ) where

import GHC

import Control.Monad.Reader

import qualified Language.Haskell.TH.Syntax as TH

import Language.Haskell.Liquid.RType

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

newtype WiredM m a = WiredM { unWiredM :: ReaderT WiredIns m a }

instance GhcMonad m => GhcMonad (WiredM m) where

runWiredM :: GhcMonad m => WiredM m a -> m a
runWiredM

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

data WiredIns =
  WiredIns
    {
    , 
    } 

loadWiredIns :: GhcMonad m => m WiredIns
loadWiredIns

