module Language.Haskell.Liquid.Parse (
    parseDecs
  , parseType
  ) where

import Language.Haskell.TH.Syntax

import Text.Parsec (many)

import Language.Haskell.Liquid.Parse.Base
import Language.Haskell.Liquid.Parse.Dec
import Language.Haskell.Liquid.Parse.Type

--------------------------------------------------------------------------------
-- Top-Level Entry Points ------------------------------------------------------
--------------------------------------------------------------------------------

parseDecs :: Bool -> String -> Q [Dec]
parseDecs simplified = runParser simplified $ concat <$> many decP

parseType :: Bool -> String -> Q (Type, [Name])
parseType simplified = runParser simplified typeP

