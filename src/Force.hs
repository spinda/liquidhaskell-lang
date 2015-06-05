module Force (forceAddTopDecls, forceGetGhcName) where

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

import qualified Language.Haskell.TH        as TH
import qualified Language.Haskell.TH.Syntax as TH

import Data.Maybe

import Unsafe.Coerce

-- A necessary evil, and a relatively safe one as far as evils go.
inTcRn :: TcRn a -> TH.Q a
inTcRn m = TH.Q $ unsafeCoerce m

-- See: https://ghc.haskell.org/trac/ghc/ticket/10486
forceAddTopDecls :: [TH.Dec] -> TH.Q ()
forceAddTopDecls decs = inTcRn $ do
  pos <- getSrcSpanM
  converted <- case convertToHsDecls pos decs of
    Left e -> pprPanic "forceAddTopDecls: can't convert top-level declarations" e
    Right ds -> return ds
  topdecls <- tcg_th_topdecls <$> getGblEnv
  updTcRef topdecls (\tds -> converted ++ tds)

forceGetGhcName :: TH.Name -> TH.Q (Maybe Name)
forceGetGhcName = inTcRn . lookupThName_maybe

