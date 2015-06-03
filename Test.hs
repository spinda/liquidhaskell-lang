{-# LANGUAGE QuasiQuotes #-}

module Test () where

import Quasi

type Nat = [lq| { v:Int | 0 <= v } |]
type Wow a = [lq| { v:a | 0 <= v } |]

[lq| id' :: a -> a |]
id' x = x

id'' :: [lq| a -> a |]
id'' x = x

[lq| add :: Nat -> Nat -> Nat |]
add x y = plus x y
  where
    plus :: [lq| Nat -> Nat -> Nat |]
    plus x y = x + y

