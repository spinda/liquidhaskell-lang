{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}

-- TODO: Rename this module

module Language.Haskell.Liquid.RType (
    -- * Location Information
    Located(..)
  , Span(..)
  , Pos(..)

    -- * Embedded Type Annotations
  , TyLocated(..)
  , TySpan(..)
  , TyPos(..)
  , Bind
  , Refine
  , ExprArgs

    -- * Embedded Refinement AST
  , Pred(..)
  , Expr(..)
  , Constant(..)
  , Brel(..)
  , Bop(..)

    -- * Type Declaration Annotations
  , ExprParams(..)
  , EmbedAs(..)
  , IsInline(..)

    -- * Components
  , FTycon(..)
  ) where

import Data.Data
import Data.Typeable

import GHC.TypeLits

--------------------------------------------------------------------------------
-- Location Information --------------------------------------------------------
--------------------------------------------------------------------------------

data Located a = Located
  { loc_span  :: Span
  , loc_value :: a
  }
  deriving (Functor, Data, Typeable)

data Span = Span
  { span_start :: Pos
  , span_end   :: Pos
  }
  deriving (Data, Typeable)

data Pos = Pos
  { pos_name  :: String
  , pos_line  :: Int
  , pos_col   :: Int
  }
  deriving (Data, Typeable)

--------------------------------------------------------------------------------
-- Embedded Type Annotations ---------------------------------------------------
--------------------------------------------------------------------------------

data TyLocated a = TyLocated TySpan a

data TySpan = TySpan TyPos TyPos

data TyPos = TyPos Symbol Nat Nat


type Bind (b :: TyLocated Symbol) a = a
type Refine a (b :: Symbol) (p :: Pred) = a

type ExprArgs a (es :: [TyLocated Expr]) = a

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
  ECon   :: Constant -> Expr
  EVar   :: Symbol -> Expr
  EParam :: Symbol -> Expr
  ECtr   :: forall a. TyLocated a -> Expr
  ENeg   :: Expr -> Expr
  EBin   :: Bop  -> Expr -> Expr -> Expr
  EIte   :: Pred -> Expr -> Expr -> Expr
  EBot   :: Expr


data Constant = I Nat

data Brel = Eq | Ne | Gt | Ge | Lt | Le | Ueq | Une

data Bop  = Plus | Minus | Times | Div | Mod

--------------------------------------------------------------------------------
-- Type Declaration Annotations ------------------------------------------------
--------------------------------------------------------------------------------

data ExprParams = ExprParams (Located [String]) deriving (Data, Typeable)

data EmbedAs = EmbedAs (Located FTycon) deriving (Data, Typeable)

data IsInline = IsInline Span deriving (Data, Typeable)

--------------------------------------------------------------------------------
-- Components ------------------------------------------------------------------
--------------------------------------------------------------------------------

data FTycon = FTcInt | FTcReal | FTcBool | FTcUser String deriving (Data, Typeable)

