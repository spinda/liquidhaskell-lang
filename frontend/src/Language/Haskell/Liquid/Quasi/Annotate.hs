{-# LANGUAGE TemplateHaskell #-}

module Language.Haskell.Liquid.Quasi.Annotate (
    mkFnSigAnnotation
  , mkTySynAnnotation
  ) where

import           Unique

import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax

import           Language.Haskell.Liquid.RType

import           Language.Haskell.Liquid.Quasi.Backdoor
import           Language.Haskell.Liquid.Quasi.Metadata

--------------------------------------------------------------------------------
-- Encode LiquidHaskell Specifications as Annotations --------------------------
--------------------------------------------------------------------------------

mkFnSigAnnotation :: Name -> AnnType -> Q Dec
mkFnSigAnnotation var at = do
  maybeName <- backdoor_getGhcName var
  case maybeName of
    Nothing -> do
      ast <- dataToExpQ (const Nothing) at
      return $ PragmaD $ AnnP (ValueAnnotation var) $ SigE ast $ ConT ''AnnType
    Just ghcName -> do
      ast <- dataToExpQ (const Nothing) $ LqLocal (getKey $ getUnique ghcName) at
      return $ PragmaD $ AnnP (TypeAnnotation ''LiquidHaskell) $ SigE ast $ ConT ''LqLocal

mkTySynAnnotation :: Name -> AnnType -> Q Dec
mkTySynAnnotation con at = do
  ast <- dataToExpQ (const Nothing) at
  return $ PragmaD $ AnnP (TypeAnnotation con) $ SigE ast $ ConT ''AnnType

