{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Haskell.Liquid.WiredIns (
    WiredM
  , runWiredM

  , WiredIns(..)
  , loadWiredIns
  ) where

import GHC hiding (L)

import ConLike
import DataCon
import DynFlags
import Exception
import HscTypes
import Panic
import TcRnMonad
import TcSplice
import TyCon

import Control.Monad.Reader

import Text.PrettyPrint.HughesPJ

import qualified Language.Haskell.TH.Ppr    as TH
import qualified Language.Haskell.TH.PprLib as TH
import qualified Language.Haskell.TH.Syntax as TH

import Language.Haskell.Liquid.RType

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

newtype WiredM m a = WiredM { unWiredM :: ReaderT WiredIns m a }
                     deriving ( Functor, Applicative, Monad
                              , MonadIO, MonadReader WiredIns
                              )

instance GhcMonad m => GhcMonad (WiredM m) where
  getSession = WiredM $ lift getSession
  setSession = WiredM . lift . setSession

instance ExceptionMonad m => ExceptionMonad (WiredM m) where
  gcatch act handle = WiredM $ do
    env <- ask
    lift $ runReaderT (unWiredM act) env
             `gcatch` \e -> runReaderT (unWiredM $ handle e) env
  gmask f = WiredM $ do
    env <- ask
    lift $ gmask $ \ghc_restore ->
      runReaderT (unWiredM $ f $ wired_restore ghc_restore) env
    where
      wired_restore ghc_restore act = WiredM $ do
         env <- ask
         lift $ ghc_restore $ runReaderT (unWiredM act) env

instance (Monad m, HasDynFlags m) => HasDynFlags (WiredM m) where
  getDynFlags = WiredM $ lift getDynFlags

runWiredM :: GhcMonad m => WiredM m a -> m a
runWiredM act = runReaderT (unWiredM act) =<< loadWiredIns

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

data WiredIns =
  WiredIns
    { tc_Bind     :: TyCon
    , tc_Refine   :: TyCon

    , pc_L        :: TyCon

    , pc_PTrue    :: TyCon
    , pc_PFalse   :: TyCon
    , pc_PAnd     :: TyCon
    , pc_POr      :: TyCon
    , pc_PNot     :: TyCon
    , pc_PImp     :: TyCon
    , pc_PIff     :: TyCon
    , pc_PExp     :: TyCon
    , pc_PAtom    :: TyCon
    , pc_PTop     :: TyCon

    , pc_ECon     :: TyCon
    , pc_EBdr     :: TyCon
    , pc_ECtr     :: TyCon
    , pc_ENeg     :: TyCon
    , pc_EBin     :: TyCon
    , pc_EIte     :: TyCon
    , pc_EBot     :: TyCon

    , pc_I        :: TyCon

    , pc_Eq       :: TyCon
    , pc_Ne       :: TyCon
    , pc_Gt       :: TyCon
    , pc_Ge       :: TyCon
    , pc_Lt       :: TyCon
    , pc_Le       :: TyCon
    , pc_Ueq      :: TyCon
    , pc_Une      :: TyCon

    , pc_Plus     :: TyCon
    , pc_Minus    :: TyCon
    , pc_Times    :: TyCon
    , pc_Div      :: TyCon
    , pc_Mod      :: TyCon
    } 

loadWiredIns :: GhcMonad m => m WiredIns
loadWiredIns = WiredIns
  <$> lookupTHTyCon ''Bind
  <*> lookupTHTyCon ''Refine

  <*> lookupPromoteTHDataCon 'L

  <*> lookupPromoteTHDataCon 'PTrue
  <*> lookupPromoteTHDataCon 'PFalse
  <*> lookupPromoteTHDataCon 'PAnd
  <*> lookupPromoteTHDataCon 'POr
  <*> lookupPromoteTHDataCon 'PNot
  <*> lookupPromoteTHDataCon 'PImp
  <*> lookupPromoteTHDataCon 'PIff
  <*> lookupPromoteTHDataCon 'PExp
  <*> lookupPromoteTHDataCon 'PAtom
  <*> lookupPromoteTHDataCon 'PTop

  <*> lookupPromoteTHDataCon 'ECon
  <*> lookupPromoteTHDataCon 'EBdr
  <*> lookupPromoteTHDataCon 'ECtr
  <*> lookupPromoteTHDataCon 'ENeg
  <*> lookupPromoteTHDataCon 'EBin
  <*> lookupPromoteTHDataCon 'EIte
  <*> lookupPromoteTHDataCon 'EBot

  <*> lookupPromoteTHDataCon 'I

  <*> lookupPromoteTHDataCon 'Eq
  <*> lookupPromoteTHDataCon 'Ne
  <*> lookupPromoteTHDataCon 'Gt
  <*> lookupPromoteTHDataCon 'Ge
  <*> lookupPromoteTHDataCon 'Lt
  <*> lookupPromoteTHDataCon 'Le
  <*> lookupPromoteTHDataCon 'Ueq
  <*> lookupPromoteTHDataCon 'Une

  <*> lookupPromoteTHDataCon 'Plus
  <*> lookupPromoteTHDataCon 'Minus
  <*> lookupPromoteTHDataCon 'Times
  <*> lookupPromoteTHDataCon 'Div
  <*> lookupPromoteTHDataCon 'Mod

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

lookupTHName :: GhcMonad m => String -> (TyThing -> Maybe a) -> TH.Name -> m a
lookupTHName desc f th = do
  hscEnv <- getSession
  name   <- maybe err return =<< (liftIO $ initTcForLookup hscEnv $ lookupThName_maybe th)
  thing  <- (f =<<) <$> lookupGlobalName name 
  maybe err return thing
  where
    err = throwGhcException $ ProgramError $
      "Not in scope: " ++ desc ++ " '" ++ render (TH.to_HPJ_Doc $ TH.ppr th) ++ "'"

lookupTHTyCon :: GhcMonad m => TH.Name -> m TyCon
lookupTHTyCon = lookupTHName "type constructor or class" f
  where
    f (ATyCon tc) = Just tc
    f _           = Nothing

lookupTHDataCon :: GhcMonad m => TH.Name -> m DataCon
lookupTHDataCon = lookupTHName "data constructor" f
  where
    f (AConLike (RealDataCon dc)) = Just dc
    f _                           = Nothing

lookupPromoteTHDataCon :: GhcMonad m => TH.Name -> m TyCon
lookupPromoteTHDataCon = fmap promoteDataCon . lookupTHDataCon

