{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Runtime (addLqAnnotation, LiquidHaskell, LqLocal(..)) where

-- TODO: Quasi will interface with an abstracted implementation that's either
--       the full thing that interacts with GHC or a stubbed-out version. The
--       former will be swapped out for the latter by the LiquidHaskell driver
--       when it's being used, and the latter will be used during normal
--       compilation. This avoids adding a dependency on GHC (and the black
--       magic tricks below) to any package using LiquidHaskell.

import Convert
import Name
import Outputable
import TcRnMonad
import TcRnTypes
import TcSplice
import TcType
import Unique

import Data.Data
import Data.Maybe
import Data.Typeable

import Unsafe.Coerce

import qualified Language.Haskell.TH        as TH
import qualified Language.Haskell.TH.Syntax as TH
import qualified Language.Haskell.TH.Quote  as TH

import RType

--------------------------------------------------------------------------------
-- Add LiquidHaskell Type Annotations ------------------------------------------
--------------------------------------------------------------------------------

addLqAnnotation :: TH.AnnTarget -> AnnType -> TH.Q ()

addLqAnnotation target@(TH.TypeAnnotation _) at = do
  ast <- TH.dataToExpQ (const Nothing) at
  backdoorAddAnnotation target $ TH.SigE ast $ TH.ConT ''AnnType

addLqAnnotation target@(TH.ValueAnnotation var) at = do
  maybeName <- backdoorGetGhcName var
  case maybeName of
    Nothing -> do
      ast <- TH.dataToExpQ (const Nothing) at
      backdoorAddAnnotation target $ TH.SigE ast $ TH.ConT ''AnnType
    Just ghcName -> do
      ast <- TH.dataToExpQ (const Nothing) $ LqLocal (getKey $ getUnique ghcName) at
      backdoorAddAnnotation (TH.TypeAnnotation ''LiquidHaskell) $ TH.SigE ast $ TH.ConT ''LqLocal

--------------------------------------------------------------------------------
-- Builtin Types for Attaching Local Annotations -------------------------------
--------------------------------------------------------------------------------

data LiquidHaskell

data LqLocal =
  LqLocal
    { lql_id :: Int
    , lql_ty :: AnnType
    }
  deriving (Data, Typeable)

--------------------------------------------------------------------------------
-- Well-Contained Interface for GHC Functionality in Q -------------------------
--------------------------------------------------------------------------------

-- A necessary evil, and a relatively safe one as far as evils go.
inTcRn :: TcRn a -> TH.Q a
inTcRn m = TH.Q $ unsafeCoerce m

-- See: https://ghc.haskell.org/trac/ghc/ticket/10486
backdoorAddTopDecls :: [TH.Dec] -> TH.Q ()
backdoorAddTopDecls decs = inTcRn $ do
  pos <- getSrcSpanM
  converted <- case convertToHsDecls pos decs of
    Left e -> pprPanic "backdoorAddTopDecls: can't convert top-level declarations" e
    Right ds -> return ds
  topdecls <- tcg_th_topdecls <$> getGblEnv
  updTcRef topdecls (\tds -> converted ++ tds)

backdoorAddAnnotation :: TH.AnnTarget -> TH.Exp -> TH.Q ()
backdoorAddAnnotation target exp = backdoorAddTopDecls [TH.PragmaD $ TH.AnnP target exp]

backdoorGetGhcName :: TH.Name -> TH.Q (Maybe Name)
backdoorGetGhcName = inTcRn . lookupThName_maybe

