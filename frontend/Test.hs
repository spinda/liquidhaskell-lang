{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Test where

import LiquidHaskell

type Nat = [lq| { v:Int | 0 <= v } |]
type Wow a = [lq| { v:a | a == 0 || a == Nothing || a == Left } |]

[lq| id' :: a -> a |]
id' x = x

id'' :: [lq| a -> a |]
id'' x = x

[lq| add :: Wow Nat -> Nat -> Nat |]
add x y = plus x y
  where
    plus :: [lq| Nat -> Nat -> Nat |]
    plus x y = x + y

