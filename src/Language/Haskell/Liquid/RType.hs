{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}

module Language.Haskell.Liquid.RType (
    -- * Embedded Type Annotations
    Bind
  , Refine
  , L(..)

    -- * Embedded Refinement AST
  , Pred(..)
  , Expr(..)
  , Constant(..)
  , Brel(..)
  , Bop(..)

    -- * FTycon Embeds
  , Embed(..)
  ) where

-- TODO: Rename this module

import Data.Data
import Data.Typeable

import GHC.TypeLits

import Language.Haskell.TH.Syntax (Name)

--------------------------------------------------------------------------------
-- Embedded Type Annotations ---------------------------------------------------
--------------------------------------------------------------------------------

type Bind (b :: Symbol) a = a
type Refine a (b :: Symbol) (p :: Pred) = a

data L a = L Symbol Nat Nat Nat Nat a

--------------------------------------------------------------------------------
-- Embedded Refinement AST -----------------------------------------------------
--------------------------------------------------------------------------------

data Pred :: * where
  PTrue  :: Pred
  PFalse :: Pred
  PAnd   :: Pred -> Pred -> Pred
  POr    :: Pred -> Pred -> Pred
  PNot   :: Pred
  PImp   :: Pred -> Pred -> Pred
  PIff   :: Pred -> Pred -> Pred
  PExp   :: Expr -> Pred
  PAtom  :: Brel -> Expr -> Expr -> Pred
  PTop   :: Pred

data Expr :: * where
  ECon :: Constant      -> Expr
  EBdr :: Symbol        -> Expr
  ECtr :: forall a. L a -> Expr
  ENeg :: Expr -> Expr
  EBin :: Bop  -> Expr -> Expr -> Expr
  EIte :: Pred -> Expr -> Expr -> Expr
  EBot :: Expr


data Constant = I Nat

data Brel = Eq | Ne | Gt | Ge | Lt | Le | Ueq | Une

data Bop  = Plus | Minus | Times | Div | Mod

--------------------------------------------------------------------------------
-- FTycon Embeds ---------------------------------------------------------------
--------------------------------------------------------------------------------

data Embed = Embed Name String deriving (Data, Typeable)

