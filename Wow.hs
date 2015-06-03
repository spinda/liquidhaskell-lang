{-# LANGUAGE TemplateHaskell #-}

module Wow where

import Language.Haskell.Meta.Parse

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote

lq :: QuasiQuoter
lq = QuasiQuoter { quoteDec  = lqDec
                 , quoteExp  = invalid
                 , quoteType = invalid
                 , quotePat  = invalid
                 }

lqDec :: String -> Q [Dec]
lqDec s =
  case parseDecs s of
    Left err ->
      fail err
    Right decs ->
      return decs

lqLocal :: QuasiQuoter
lqLocal = QuasiQuoter { quoteType = lqLocalType
                      , quoteExp  = invalid
                      , quotePat  = invalid
                      , quoteDec  = invalid
                      }

lqLocalType :: String -> Q Type
lqLocalType s = do
  let (name, sig) = break (== '|') s
  Just name' <- lookupValueName name
  runIO $ putStrLn $ show name'
  case parseType $ drop 1 sig of
    Left err ->
      fail err
    Right ty ->
      return ty

invalid :: String -> Q a
invalid _ = do
  loc <- location
  fail $ "invalid context for lq: " ++ pprint loc ++ "; must generate declarations or complete type in signature"

