module Language.Haskell.Liquid.Quasi (
    lq
  ) where

import           Data.List

import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax

import           Language.Haskell.Liquid.Parser
import           Language.Haskell.Liquid.RType

import           Language.Haskell.Liquid.Quasi.Annotate
import           Language.Haskell.Liquid.Quasi.Backdoor

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

ofDec (TySyn con tvs ty) = do
  pragma <- mkTySynAnnotation con at
  return [pragma, TySynD con tvs st]
  where
    (st, at) = splitRTy ty

ofDec (FnSig var ty) = do
  pragma <- mkFnSigAnnotation var at
  return [pragma, SigD var st]
  where
    (st, at) = splitRTy ty

--------------------------------------------------------------------------------
-- Specialized Type Context Handling -------------------------------------------
--------------------------------------------------------------------------------

lqType :: String -> Q Type
lqType s = do
  (ty, tvs) <- parseType $ drop 2 sig
  case sig !! 1 of
    't' -> do
      let (st, at) = splitRTy ty
      Just name <- lookupTypeName id
      pragma    <- mkTySynAnnotation name at
      backdoor_addTopDecls [pragma]
      return st
    'v' -> do
      let ty' = quantifyRTy tvs ty
      let (st, at) = splitRTy ty'
      Just name <- lookupValueName id
      pragma    <- mkFnSigAnnotation name at
      backdoor_addTopDecls [pragma]
      return st
  where
    (id, sig) = break (== '|') s

