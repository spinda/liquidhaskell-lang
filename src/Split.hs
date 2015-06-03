module Split where

import Data.List

import Language.Haskell.TH.Syntax

import RType

splitType :: QuasiType -> (Type, AnnType)
splitType t = (simplType t, stripType t)


simplType :: QuasiType -> Type
simplType (RVar tv _)
  = VarT tv
simplType (RAppTy t1 t2 _)
  = AppT (simplType t1) (simplType t2)
simplType (RApp tc as _)
  = foldl' AppT (ConT tc) (map simplType as)
simplType (RFun _ i o _)
  = ArrowT `AppT` simplType i `AppT` simplType o
simplType (RAllT tv ty)
  = let (tvs, rest) = collectTVs [tv] ty
    in  ForallT (map PlainTV $ reverse tvs) [] (simplType rest)

collectTVs :: [Name] -> QuasiType -> ([Name], QuasiType)
collectTVs tvs (RAllT tv rest)
  = collectTVs (tv:tvs) rest
collectTVs tvs rest
  = (tvs, rest)


stripType :: QuasiType -> AnnType
stripType (RVar _ r)
  = RVar () r
stripType (RAppTy t1 t2 r)
  = RAppTy (stripType t1) (stripType t2) r
stripType (RApp _ as r)
  = RApp () (map stripType as) r
stripType (RFun b i o r)
  = RFun b (stripType i) (stripType o) r
stripType (RAllT _ ty)
  = RAllT () (stripType ty)

