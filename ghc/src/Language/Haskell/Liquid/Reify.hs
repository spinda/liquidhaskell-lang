{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Language.Haskell.Liquid.Reify (
    reifyRTy
  , reifyRReft
  , reifyReft
  , reifyRefa
  , reifyPred
  , reifyExpr
  ) where

import GHC hiding (Located)

import ConLike
import DataCon
import DynFlags
import FastString
import IdInfo
import Name
import Panic
import TyCon
import Type
import TypeRep
import Unique
import Var

import qualified Outputable as Out

import Control.Monad.Reader

import Data.Monoid

import Text.Parsec.Pos

import Language.Fixpoint.Names
import Language.Fixpoint.Types

import Language.Haskell.Liquid.Types
import Language.Haskell.Liquid.WiredIns

--------------------------------------------------------------------------------
-- Reify RType -----------------------------------------------------------------
--------------------------------------------------------------------------------

-- FIXME: Application edge-cases

reifyRTy :: GhcMonad m => Type -> WiredM m SpecType

reifyRTy (TyVarTy tv)  =
  return $ RVar tv mempty

reifyRTy (AppTy t1 t2) =
  RAppTy <$> reifyRTy t1 <*> reifyRTy t2 <*> pure mempty

reifyRTy ty@(TyConApp tc as) = go =<< ask
  where
    go wis
      | tc == tc_Refine wis, [a, b, p] <- as =
        strengthen <$> reifyRTy a <*> reifyRReft b p
      | Just (tenv, rhs, as') <- tcExpandTyCon_maybe tc as =
        reifyRTy $ mkAppTys (substTy (mkTopTvSubst tenv) rhs) as'
      | otherwise =
        RApp tc <$> mapM reifyRTy as <*> pure mempty

reifyRTy (FunTy i o) = do
  (b, i') <- reifyBind i
  RFun b <$> reifyRTy i <*> reifyRTy o <*> pure mempty

reifyRTy (ForAllTy tv ty) = do
  RAllT tv <$> reifyRTy ty

reifyRTy ty@(LitTy _)  =
  malformed "type" ty


-- TODO: Move
strengthen :: Reftable r => RType c tv r -> r -> RType c tv r
strengthen (RApp c ts    r) r'  = RApp c ts    (r `meet` r')
strengthen (RVar a r) r'        = RVar a       (r `meet` r')
strengthen (RFun b t1 t2 r) r'  = RFun b t1 t2 (r `meet` r')
strengthen (RAppTy t1 t2 r) r'  = RAppTy t1 t2 (r `meet` r')
strengthen t _                  = t

--------------------------------------------------------------------------------
-- Reify RReft -----------------------------------------------------------------
--------------------------------------------------------------------------------

reifyRReft :: GhcMonad m => Type -> Type -> WiredM m RReft
reifyRReft = reifyReft

--------------------------------------------------------------------------------
-- Reify Reft -----------------------------------------------------------------
--------------------------------------------------------------------------------

reifyReft :: GhcMonad m => Type -> Type -> WiredM m Reft
reifyReft b r = do
  b' <- reifySymbol b
  r' <- reifyRefa r
  return $ Reft (b', r')

--------------------------------------------------------------------------------
-- Reify Refa -----------------------------------------------------------------
--------------------------------------------------------------------------------

reifyRefa :: GhcMonad m => Type -> WiredM m Refa
reifyRefa = fmap Refa . reifyPred

--------------------------------------------------------------------------------
-- Reify Pred -----------------------------------------------------------------
--------------------------------------------------------------------------------

reifyPred :: GhcMonad m => Type -> WiredM m Pred
reifyPred ty = (`go` ty) =<< ask
  where
    go wis (TyConApp tc as)
      | tc == pc_PTrue wis, [] <- as =
        return PTrue
      | tc == pc_PFalse wis, [] <- as =
        return PFalse
      | tc == pc_PAnd wis, [x, y] <- as =
        PAnd <$> mapM reifyPred [x, y]
      | tc == pc_POr wis, [x, y] <- as =
        POr <$> mapM reifyPred [x, y]
      | tc == pc_PNot wis, [x] <- as =
        PNot <$> reifyPred x
      | tc == pc_PImp wis, [x, y] <- as =
        PImp <$> reifyPred x <*> reifyPred y
      | tc == pc_PIff wis, [x, y] <- as =
        PIff <$> reifyPred x <*> reifyPred y
      | tc == pc_PExp wis, [x] <- as =
        PBexp <$> reifyExpr x
      | tc == pc_PAtom wis, [brel, e1, e2] <- as =
        PAtom <$> reifyBrel brel <*> reifyExpr e1 <*> reifyExpr e2
      | tc == pc_PTop wis, [] <- as =
        return PTop
    go _ _ = malformed "predicate" ty

--------------------------------------------------------------------------------
-- Reify Expr ------------------------------------------------------------------
--------------------------------------------------------------------------------

reifyExpr :: GhcMonad m => Type -> WiredM m Expr
reifyExpr ty = (`go` ty) =<< ask
  where
    go wis (TyConApp tc as)
      | tc == pc_ECon wis, [c] <- as =
        ECon <$> reifyConstant c
      | tc == pc_EBdr wis, [s] <- as =
        EVar <$> reifySymbol s
      | tc == pc_ECtr wis, [_, t] <- as =
        (EVar . val) <$> reifyLocated reifyDataCon t
      | tc == pc_ENeg wis, [e] <- as =
        ENeg <$> reifyExpr e
      | tc == pc_EBin wis, [bop, e1, e2] <- as =
        EBin <$> reifyBop bop <*> reifyExpr e1 <*> reifyExpr e2
      | tc == pc_EIte wis, [pred, e1, e2] <- as =
        EIte <$> reifyPred pred <*> reifyExpr e1 <*> reifyExpr e2
      | tc == pc_EBot wis, [] <- as =
        return EBot
    go _ _ = malformed "expression" ty


reifyConstant :: GhcMonad m => Type -> WiredM m Constant
reifyConstant ty = (`go` ty) =<< ask
  where
    go wis (TyConApp tc [a])
      | tc == pc_I wis =
        I <$> reifyNat a
    go _ _ = malformed "constant" ty

reifyBrel :: GhcMonad m => Type -> WiredM m Brel
reifyBrel ty = (`go` ty) =<< ask
  where
    go wis (TyConApp tc [])
      | tc == pc_Eq  wis = return Eq
      | tc == pc_Ne  wis = return Ne
      | tc == pc_Gt  wis = return Gt
      | tc == pc_Ge  wis = return Ge
      | tc == pc_Lt  wis = return Lt
      | tc == pc_Le  wis = return Le
      | tc == pc_Ueq wis = return Ueq
      | tc == pc_Une wis = return Une
    go _ _ = malformed "binary relation" ty

reifyBop :: GhcMonad m => Type -> WiredM m Bop
reifyBop ty = (`go` ty) =<< ask
  where
    go wis (TyConApp tc [])
      | tc == pc_Plus  wis = return Plus
      | tc == pc_Minus wis = return Minus
      | tc == pc_Times wis = return Times
      | tc == pc_Div   wis = return Div
      | tc == pc_Mod   wis = return Mod
    go _ _ = malformed "binary operator" ty

--------------------------------------------------------------------------------
-- Reify Data Constructor ------------------------------------------------------
--------------------------------------------------------------------------------

reifyDataCon :: GhcMonad m => Type -> WiredM m Symbol
reifyDataCon (TyConApp tc _)
  | Just dc <- isPromotedDataCon_maybe tc =
    return $ varSymbol $ dataConWorkId dc
reifyDataCon ty =
  malformed "data constructor" ty


-- TODO: Move

varSymbol :: Var -> Symbol
varSymbol v
  | us `isSuffixOfSym` vs = vs
  | otherwise             = vs `mappend` singletonSym symSepName `mappend` us
  where us  = symbol $ showPpr $ getDataConVarUnique v
        vs  = symbol $ showPpr $ getName v

showPpr :: Out.Outputable a => a -> String
showPpr = showSDoc . Out.ppr

showSDoc :: Out.SDoc -> String
showSDoc sdoc = Out.renderWithStyle unsafeGlobalDynFlags sdoc (Out.mkUserStyle Out.alwaysQualify Out.AllTheWay)

getDataConVarUnique :: Var -> Unique
getDataConVarUnique v
  | isId v && isDataConId v = getUnique $ idDataCon v
  | otherwise               = getUnique v

isDataConId :: Id -> Bool
isDataConId id = case idDetails id of
  DataConWorkId _ -> True
  DataConWrapId _ -> True
  _               -> False

--------------------------------------------------------------------------------
-- Reify Components ------------------------------------------------------------
--------------------------------------------------------------------------------

reifyBind :: GhcMonad m => Type -> WiredM m (Symbol, Type)
reifyBind ty = (`go` ty) =<< ask
  where
    go wis (TyConApp tc [b, a])
      | tc == tc_Bind wis = (, a) <$> reifySymbol b
    -- FIXME: Generate a fresh symbol for each default binder
    go _ _ = return (tempSymbol "db" 0, ty)


reifyLocated :: GhcMonad m => (Type -> WiredM m a) -> Type -> WiredM m (Located a)
reifyLocated f ty = (`go` ty) =<< ask
  where
    go wis (TyConApp tc [_, filename, startLine, startCol, endLine, endCol, x])
      | tc == pc_L wis = do
        filename'  <- reifyString filename
        startLine' <- fromIntegral <$> reifyNat startLine
        startCol'  <- fromIntegral <$> reifyNat startCol
        endLine'   <- fromIntegral <$> reifyNat endLine
        endCol'    <- fromIntegral <$> reifyNat endCol
        x'         <- f x
        return $ Loc (newPos filename' startLine' startCol')
                     (newPos filename' endLine'   endCol'  )
                     x'
    go _ _ = malformed "location annotation" ty

reifyLocSymbol :: GhcMonad m => Type -> WiredM m LocSymbol
reifyLocSymbol = reifyLocated reifySymbol


reifyString :: GhcMonad m => Type -> WiredM m String
reifyString (LitTy (StrTyLit s)) = return $ unpackFS s
reifyString ty                   = malformed "symbol" ty

reifySymbol :: GhcMonad m => Type -> WiredM m Symbol
reifySymbol = fmap symbol . reifyString

reifyNat :: GhcMonad m => Type -> WiredM m Integer
reifyNat (LitTy (NumTyLit n)) = return n
reifyNat ty                   = malformed "natural number" ty

--------------------------------------------------------------------------------
-- Utility Functions -----------------------------------------------------------
--------------------------------------------------------------------------------

malformed :: GhcMonad m => String -> Type -> m a
malformed desc ty = do
  dflags <- getSessionDynFlags
  let str = Out.showSDoc dflags $ Out.ppr ty
  panic $ "Malformed LiquidHaskell " ++ desc ++ " encoding: " ++ str

