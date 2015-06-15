module LiquidHaskell (
    lq
  ) where

import           Control.Monad
import           Data.Maybe

import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax

import           Language.Haskell.Liquid.Parse
import           Language.Haskell.Liquid.RType
import           Language.Haskell.Liquid.Util

--------------------------------------------------------------------------------
-- Top-Level QuasiQuoter Entry Point -------------------------------------------
--------------------------------------------------------------------------------

lq :: QuasiQuoter
lq = QuasiQuoter { quoteType = lqType
                 , quoteDec  = lqDec
                 , quoteExp  = lqInvalid "expression"
                 , quotePat  = lqInvalid "pattern"
                 }

lqInvalid :: String -> String -> Q a
lqInvalid ctxt _ = fail $
  "`lq` quasiquoter cannot be used in the " ++ ctxt ++ " context"

--------------------------------------------------------------------------------
-- Specialized Context Handling ------------------------------------------------
--------------------------------------------------------------------------------

lqDec :: String -> Q [Dec]
lqDec = parseDecs

lqType :: String -> Q Type
lqType s = do
  (ty, tvs) <- parseType s
  newTVs    <- filterM (fmap isNothing . lookupTypeName . nameBase) tvs
  return $ quantifyTy newTVs ty

