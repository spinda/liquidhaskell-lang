{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Haskell.Liquid.Resolve (
    resolveRTy
  ) where

import GHC hiding (Located)

import ConLike
import DataCon
import DynFlags
import Exception
import IdInfo
import MonadUtils
import Name
import Unique
import Var

import qualified Outputable as Out

import Control.Monad.Reader

import Data.Char
import Data.List
import Data.Maybe
import Data.Traversable
import Data.Typeable

import qualified Data.HashSet as S

import qualified Language.Haskell.TH.Syntax as TH

import Language.Haskell.Liquid.Fixpoint
import Language.Haskell.Liquid.RType
import Language.Haskell.Liquid.SpecType

--------------------------------------------------------------------------------
-- Resolve Names in Refinements ------------------------------------------------
--------------------------------------------------------------------------------

resolveRTy :: GhcMonad m => [String] -> TH.Loc -> SpecType -> m SpecType
resolveRTy eargs l ty = runReaderT (resolve l ty) (S.fromList eargs)

--------------------------------------------------------------------------------
-- Internal Resolve Traversal --------------------------------------------------
--------------------------------------------------------------------------------

type ResolveT = ReaderT (S.HashSet String)


class Resolve a where
  resolve :: GhcMonad m => TH.Loc -> a -> ResolveT m a


instance Resolve a => Resolve (Located a) where
  resolve _ (Loc l x) = Loc l <$> resolve l x


instance Resolve SpecType where
  resolve l = traverse (resolve l)

instance Resolve Reft where
  resolve l (Reft (s, ra)) = Reft . (s, ) <$> resolve l ra

instance Resolve Refa where
  resolve l (Refa p) = Refa <$> resolve l p


instance Resolve Pred where
  resolve l (PAnd  ps     ) = PAnd      <$> mapM (resolve l) ps
  resolve l (POr   ps     ) = POr       <$> mapM (resolve l) ps
  resolve l (PNot  p      ) = PNot      <$> resolve l p
  resolve l (PImp  p  q   ) = PImp      <$> resolve l p  <*> resolve l q
  resolve l (PIff  p  q   ) = PIff      <$> resolve l p  <*> resolve l q
  resolve l (PBexp b      ) = PBexp     <$> resolve l b
  resolve l (PAtom r e1 e2) = PAtom r   <$> resolve l e1 <*> resolve l e2
  resolve l (PAll  vs p   ) = PAll      <$> mapM (mapM (resolve l)) vs <*> resolve l p
  resolve _ p               = return p

instance Resolve Expr where
  resolve l (EVar s)       = do
    liftIO $ putStrLn $ "var: " ++ s
    EVar   <$> resolve l s
  resolve l (EApp s es)    = EApp   <$> resolve l s  <*> mapM (resolve l) es
  resolve l (ENeg e)       = ENeg   <$> resolve l e
  resolve l (EBin o e1 e2) = EBin o <$> resolve l e1 <*> resolve l e2
  resolve l (EIte p e1 e2) = EIte   <$> resolve l p  <*> resolve l e1 <*> resolve l e2
  resolve l (ECst x s)     = ECst   <$> resolve l x  <*> resolve l s
  resolve _ e              = return e

instance Resolve Sort where
  resolve _ FInt         = return FInt
  resolve _ FReal        = return FReal
  resolve _ FNum         = return FNum
  resolve _ FFrac        = return FFrac
  resolve _ s@(FObj _)   = return s
  resolve _ s@(FVar _)   = return s
  resolve l (FFunc i ss) = FFunc i <$> mapM (resolve l) ss
  resolve _ (FApp _tc _ss) = error "TODO: Resolve FApp"


instance Resolve String where
  resolve l s
    | s `elem` prims =
      return s
    | isCon s = do
      eargs <- ask
      if s `S.member` eargs
         then return s
         else varSymbol <$> lift (lookupGhcVar (Loc l s))
    | otherwise =
      return s

isCon :: String -> Bool
isCon []    = False
isCon (c:_) = isUpper c

varSymbol :: Var -> String
varSymbol v
  | us `isSuffixOf` vs = vs
  | otherwise          = vs ++ '#':us
  where us  = showPpr $ getDataConVarUnique v
        vs  = showPpr $ getName v

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


lookupGhcVar :: GhcMonad m => LocSymbol -> m Var
lookupGhcVar (Loc l x) = do
  names  <- parseName x
  things <- mapMaybeM lookupName names
  let vars = mapMaybe ofThing things
  case vars of
    (var:_) -> return var
    []      -> liftIO $ throwIO $ ProgramError $ show l ++ ": Not in scope: variable `" ++ x ++ "`"
  where
    ofThing (AnId x)                   = Just x
    ofThing (AConLike (RealDataCon x)) = Just $ dataConWorkId x
    ofThing _                          = Nothing

