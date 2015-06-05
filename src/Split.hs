module Split (splitRTy) where

import Data.List

import Language.Haskell.TH.Syntax

import RType

--------------------------------------------------------------------------------

splitRTy :: QuasiType -> (Type, AnnType)
splitRTy t = (simplRTy t, stripRTy t)

--------------------------------------------------------------------------------

simplRTy :: QuasiType -> Type
simplRTy (RVar tv _)
  = VarT tv
simplRTy (RAppTy t1 t2 _)
  = AppT (simplRTy t1) (simplRTy t2)
simplRTy (RApp tc as _)
  = foldl' AppT (ConT tc) (map simplRTy as)
simplRTy (RFun _ i o _)
  = ArrowT `AppT` simplRTy i `AppT` simplRTy o
simplRTy (RAllT tv ty)
  = let (tvs, rest) = collectTVs [tv] ty
    in  ForallT (map PlainTV $ reverse tvs) [] (simplRTy rest)

collectTVs :: [Name] -> QuasiType -> ([Name], QuasiType)
collectTVs tvs (RAllT tv rest)
  = collectTVs (tv:tvs) rest
collectTVs tvs rest
  = (tvs, rest)

--------------------------------------------------------------------------------

stripRTy :: QuasiType -> AnnType
stripRTy (RVar _ r)
  = RVar () r
stripRTy (RAppTy t1 t2 r)
  = RAppTy (stripRTy t1) (stripRTy t2) r
stripRTy (RApp _ as r)
  = RApp () (map stripRTy as) r
stripRTy (RFun b i o r)
  = RFun b (stripRTy i) (stripRTy o) r
stripRTy (RAllT _ ty)
  = RAllT () (stripRTy ty)

