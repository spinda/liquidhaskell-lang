{-# LANGUAGE TemplateHaskell #-}

module Quasi (lq) where

import Unique

import Data.List

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote

import Force
import Split
import Syn
import Parser
import RType

lq :: QuasiQuoter
lq = QuasiQuoter { quoteType = lqType
                 , quoteDec  = lqDec
                 , quoteExp  = lqInvalid "expression"
                 , quotePat  = lqInvalid "pattern"
                 }

lqDec :: String -> Q [Dec]
lqDec s = case parseDecs s of
  Left  err  -> fail err
  Right decs -> concat <$> mapM ofDec decs

ofDec :: ParsedDec -> Q [Dec]

ofDec (TySyn con tvs ty) = do
  con' <- newName con
  ast  <- dataToExpQ (const Nothing) at
  return
    [ PragmaD $ AnnP (TypeAnnotation con') $ SigE ast $ ConT ''AnnType
    , TySynD con' tvs st
    ]
  where
    (st, at) = splitType ty

ofDec (FnSig var ty) = do
  ast <- dataToExpQ (const Nothing) at
  return
    [ PragmaD $ AnnP (ValueAnnotation var) $ SigE ast $ ConT ''AnnType
    , SigD var st
    ]
  where
    (st, at) = splitType ty

lqType :: String -> Q Type
lqType s = do
  case parseType $ drop 2 sig of
    Left err ->
      fail err
    Right (ty, tvs) -> do
      case head $ drop 1 sig of
        'v' -> do
          let ty' = quantify tvs ty
          let (st, at) = splitType ty'
          Just name <- lookupValueName id
          Just ghcName <- forceGetGhcName name
          ast <- dataToExpQ (const Nothing) $ LqLocal (getKey $ getUnique ghcName) at
          forceAddTopDecls [PragmaD $ AnnP (TypeAnnotation ''LiquidHaskell) $ SigE ast $ ConT ''LqLocal]
          return st
        't' -> do
          let (st, at) = splitType ty
          ast <- dataToExpQ (const Nothing) at
          Just name <- lookupTypeName id
          forceAddTopDecls [PragmaD $ AnnP (TypeAnnotation name) $ SigE ast $ ConT ''AnnType]
          return st
  where
    (id, sig) = break (== '|') s

lqInvalid :: String -> String -> Q a
lqInvalid ctxt _ = fail $
  "`lq` quasiquoter cannot be used in the " ++ ctxt ++ " context"
