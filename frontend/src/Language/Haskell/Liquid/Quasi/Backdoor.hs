module Language.Haskell.Liquid.Quasi.Backdoor (
    backdoor_addTopDecls
  , backdoor_getGhcName
  ) where

import           Convert
import           Name
import           Outputable
import           TcRnMonad
import           TcRnTypes
import           TcSplice
import           TcType

import           Unsafe.Coerce

import qualified Language.Haskell.TH           as TH
import qualified Language.Haskell.TH.Syntax    as TH

import           Language.Haskell.Liquid.RType

--------------------------------------------------------------------------------
-- Contained Interface for GHC Functionality in Q ------------------------------
--------------------------------------------------------------------------------

-- A necessary evil, and a relatively safe one as far as evils go.
inTcRn :: TcRn a -> TH.Q a
inTcRn m = TH.Q $ unsafeCoerce m

-- See: https://ghc.haskell.org/trac/ghc/ticket/10486
backdoor_addTopDecls :: [TH.Dec] -> TH.Q ()
backdoor_addTopDecls decs = inTcRn $ do
  pos <- getSrcSpanM
  converted <- case convertToHsDecls pos decs of
    Left e -> pprPanic "backdoorAddTopDecls: can't convert top-level declarations" e
    Right ds -> return ds
  topdecls <- tcg_th_topdecls <$> getGblEnv
  updTcRef topdecls (\tds -> converted ++ tds)

backdoor_getGhcName :: TH.Name -> TH.Q (Maybe Name)
backdoor_getGhcName = inTcRn . lookupThName_maybe

