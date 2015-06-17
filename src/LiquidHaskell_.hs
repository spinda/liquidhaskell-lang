module LiquidHaskell_ (
    lq
  ) where

import           Language.Haskell.TH.Quote
import qualified Language.Haskell.Liquid.Quasi as LH

lq :: QuasiQuoter
lq = LH.lq False

