module Lanugage.Haskell.Liquid.Reify (
    reifyRTy
  , reifyRReft
  , reifyReft
  , reifyPred
  , reifyExpr
  ) where

import GHC

import Name
import TyCon
import Type
import TypeRep
import Var

import Language.Fixpoint.Types

import Language.Haskell.Liquid.Types
import Language.Haskell.Liquid.WiredIns

--------------------------------------------------------------------------------
-- Reify RType -----------------------------------------------------------------
--------------------------------------------------------------------------------

reifyRTy :: GhcMonad m => Type -> WiredM m SpecType
reifyRTy

--------------------------------------------------------------------------------
-- Reify RReft -----------------------------------------------------------------
--------------------------------------------------------------------------------

reifyRReft :: GhcMonad m => Type -> WiredM m RReft
reifyRReft = reifyReft

--------------------------------------------------------------------------------
-- Reify Reft -----------------------------------------------------------------
--------------------------------------------------------------------------------

reifyReft :: GhcMonad m => Type -> WiredM m Reft
reifyReft

--------------------------------------------------------------------------------
-- Reify Pred -----------------------------------------------------------------
--------------------------------------------------------------------------------

reifyPred :: GhcMonad m => Type -> WiredM m Pred
reifyPred

--------------------------------------------------------------------------------
-- Reify Expr ------------------------------------------------------------------
--------------------------------------------------------------------------------

reifyExpr :: GhcMonad m => Type -> WiredM m Expr
reifyExpr
