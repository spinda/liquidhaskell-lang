module Force (forceAddTopDecls, forceGetGhcName) where

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

inTcRn :: TcRn a -> TH.Q a
inTcRn m = TH.Q $ unsafeCoerce m

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

