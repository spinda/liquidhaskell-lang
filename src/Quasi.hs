{-# LANGUAGE TemplateHaskell #-}

module Quasi (lq) where

import Name
import Unique

import Data.List

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote

import RType
import Runtime
import Split
import Parser

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
  addLqAnnotation (TypeAnnotation con) at
  return [TySynD con tvs st]
  where
    (st, at) = splitRTy ty

ofDec (FnSig var ty) = do
  addLqAnnotation (ValueAnnotation var) at
  return [SigD var st]
  where
    (st, at) = splitRTy ty

--------------------------------------------------------------------------------
-- Specialized Type Context Handling -------------------------------------------
--------------------------------------------------------------------------------

lqType :: String -> Q Type
lqType s = do
  (ty, tvs) <- parseType $ drop 2 sig
  case head $ drop 1 sig of
    'v' -> do
      let ty' = quantify tvs ty
      let (st, at) = splitRTy ty'
      Just name <- lookupValueName id
      addLqAnnotation (ValueAnnotation name) at
      return st
    't' -> do
      let (st, at) = splitRTy ty
      Just name <- lookupTypeName id
      addLqAnnotation (TypeAnnotation name) at
      return st
  where
    (id, sig) = break (== '|') s


