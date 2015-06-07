module LiquidHaskell (
    lq
  ) where

import           Control.Monad
import           Data.Maybe

import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax

import           Language.Haskell.Liquid.Parser
import           Language.Haskell.Liquid.RType

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
-- Specialized Declaration Context Handling ------------------------------------
--------------------------------------------------------------------------------

lqDec :: String -> Q [Dec]
lqDec s = concat <$> (mapM ofDec =<< parseDecs s)

ofDec :: ParsedDec -> Q [Dec]
ofDec (TySyn con tvs ty) = return [TySynD con tvs $ simplRTy ty]
ofDec (FnSig var     ty) = return [SigD   var     $ simplRTy ty]

--------------------------------------------------------------------------------
-- Specialized Type Context Handling -------------------------------------------
--------------------------------------------------------------------------------

lqType :: String -> Q Type
lqType s = do
  (ty, tvs) <- parseType s
  newTVs    <- filterM (fmap isNothing . lookupTypeName . nameBase) tvs
  return $ simplRTy $ quantifyRTy newTVs ty

