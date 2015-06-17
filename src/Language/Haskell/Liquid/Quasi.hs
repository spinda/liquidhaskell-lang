module Language.Haskell.Liquid.Quasi (
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

lq :: Bool -> QuasiQuoter
lq simpl =
  QuasiQuoter
    { quoteType = lqType simpl
    , quoteDec  = lqDec  simpl
    , quoteExp  = lqInvalid "expression"
    , quotePat  = lqInvalid "pattern"
    }

lqInvalid :: String -> String -> Q a
lqInvalid ctxt _ = fail $
  "`lq` quasiquoter cannot be used in the " ++ ctxt ++ " context"


lqDec :: Bool -> String -> Q [Dec]
lqDec = parseDecs

lqType :: Bool -> String -> Q Type
lqType simpl s = do
  (ty, tvs) <- parseType simpl s
  newTVs    <- filterM (fmap isNothing . lookupTypeName . nameBase) tvs
  return $ quantifyTy newTVs ty

