module Lanugage.Haskell.Liquid.WiredIns (
    WiredM
  , runWiredM

  , WiredIns(..)
  , loadWiredIns
  ) where

import GHC

import ConLike

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

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

lookupTHName :: GhcMonad m => String -> (TyThing -> Maybe a) -> TH.Name -> m a
lookupTHName

lookupTHTyCon :: GhcMonad m => TH.Name -> m TyCon
lookupTHTyCon = lookupTHName "type constructor" f
  where
    f (ATyCon tc) = Just tc
    f _           = Nothing

lookupTHDataCon :: GhcMonad m => TH.Name -> m TyCon
lookupTHDataCon = lookupTHName "data constructor" f
  where
    f (AConLike (RealDataCon dc)) = Just dc
    f _                           = Nothing

