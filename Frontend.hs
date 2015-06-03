{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}

import GHC
import GHC.Desugar
import GHC.Paths (libdir)

import Annotations
import Bag
import Convert
import CoreMonad
import Digraph
import DynFlags
import GhcMonad
import DynFlags
import FastString
import HscMain
import HscTypes
import MonadUtils
import Name
import OccName
import Outputable
import RdrName
import Serialized
import TcRnDriver
import TyCon
import Type
import UniqFM
import Unique
import Var

import HsBinds
import HsDecls
import HsTypes

import Control.Monad

import Data.Data
import Data.Maybe
import Data.Typeable
import Data.Word

import Data.Generics.Aliases
import Data.Generics.Schemes
import Data.Generics.Text

import qualified Data.IntMap.Strict as M

import qualified Language.Haskell.TH.Syntax as TH

import RType
import Syn
import TyUnify

--------------------------------------------------------------------------------

main :: IO ()
main = 
  defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    runGhc (Just libdir) $ do
      dflags <- getSessionDynFlags
      setSessionDynFlags $ dflags { ghcLink = LinkInMemory
                                  , hscTarget = HscInterpreted
                                  , ghcMode = CompManager
                                  }
      target <- guessTarget "Test.hs" Nothing
      setTargets [target]
      graph <- depanal [] True
      typeData <- concatMapM extractTypeData $ flattenSCCs $ topSortModuleGraph False graph Nothing
      let unified = map (\(name, ty, ann) -> (name, unifyType ty ann)) typeData

      liftIO $ mapM_ (putStrLn . skel) unified

--------------------------------------------------------------------------------

extractTypeData :: GhcMonad m => ModSummary -> m [(Name, Type, AnnType)]
extractTypeData summ = do
  parsed <- parseModule summ
  let parsed' = parsed { pm_parsed_source = editQuasiQuotes $ pm_parsed_source parsed }
  typechecked <- typecheckModule parsed'
  desugared <- desugarModule typechecked
  loadModule desugared
  setContext [IIModule $ moduleName $ ms_mod summ]

  let varMap = buildVarMap $ tm_typechecked_source typechecked

  let topLevelAnns = annotationsOfType $ dm_core_module desugared
  -- TODO: Only extract annotations on 'LiquidHaskell here
  let localAnns = annotationsOfType $ dm_core_module desugared

  topLevel <- mapM extractTopLevel topLevelAnns
  local    <- mapM (extractLocal varMap) $ map snd localAnns
  return $ topLevel ++ local

extractTopLevel :: GhcMonad m => (Name, AnnType) -> m (Name, Type, AnnType)
extractTopLevel (name, ty) = do
  Just thing <- lookupName name
  return (name, ofThing thing, ty)
  where
    ofThing (AnId id) =
      idType id
    ofThing (AConLike _) = 
      error "TODO: unexpected AConLike"
    ofThing (ATyCon tc) = case synTyConRhs_maybe tc of
      Just rhs ->
        rhs
      Nothing ->
        error "TODO: unexpected non-synonym ATyCon"
    ofThing (ACoAxiom _) =
      error "TODO: unexpected ACoAxiom"

extractLocal :: GhcMonad m => M.IntMap Id -> LqLocal -> m (Name, Type, AnnType)
extractLocal varMap (LqLocal id ty) = do
  let Just var = M.lookup id varMap
  return (getName var, varType var, ty)

--------------------------------------------------------------------------------

{-
resolveType :: [(Name, AnnType)] -> (Name, Type, AnnType) -> (Name, SpecType)
resolveType scope (name, ghcType, annType) =
  (name, resolveType' scope ghcType annType)
  
resolveType' :: [(Name, AnnType)] -> Type -> AnnType -> SpecType

resolveType' scope (TyConApp tc as) (RApp _ as' r)
  | Just (tenv, rhs, tys') <- tcExpandTyCon_maybe tc tys
  = resolveType' (mkAppTys (substTy (mkTopTvSubst tenv) rhs) tys') (mkRAppTys $ lookup (yConName)
  | otherwidse
  = RApp tc (zipWith resolveType' as as') r

resolveType' _ (TyVarTy tv) (RVar _ r) = 
  RVar tv r
resolveType' scope (AppTy t1 t2) (RAppTy t1' t2' r) =
  RAppTy (resolveType' scope t1 t1') (resolveType' scope t2 t2') r
resolveType' (FunTy i o) (RFun b i' o' r) =
  RFun b (resolveType' scope i i') (resolveType' scope o o') r
resolveType' (ForAllTy tv ty) (RAllT _ ty') =
  RAllT tv (resolveType' scope ty ty')
resolveType' (LitTy _) _ =
  error "TODO: LiquidHaskell doesn't support type-level literals, yet"
-}

--------------------------------------------------------------------------------

annotationsOfType :: (Data a, Typeable a) => ModGuts -> [(Name, a)]
annotationsOfType = mapMaybe annotationOfType . mg_anns

annotationOfType :: (Data a, Typeable a) => Annotation -> Maybe (Name, a)
annotationOfType (Annotation (NamedTarget name) payload) =
  (name, ) <$> fromSerialized deserializeWithData payload
annotationOfType _ = Nothing

--------------------------------------------------------------------------------

-- TODO: Only edit `lq` quotes!
-- TODO: Combine these syb traversals
-- FIXME: This area is very ugly!
editQuasiQuotes :: ParsedSource -> ParsedSource
editQuasiQuotes =
  everywhere (mkT editSigQuote) . everywhere (mkT editSynQuote)

editSigQuote :: Sig RdrName -> Sig RdrName
editSigQuote (TypeSig [name] (L l (HsForAllTy Implicit sp tvb ctxt (L l' (HsQuasiQuoteTy (HsQuasiQuote id pos fs))))) post) =
  TypeSig [name] (L l $ HsForAllTy Implicit sp tvb ctxt $ L l' $ HsQuasiQuoteTy $ HsQuasiQuote id pos $ appendFS (appendFS (occNameFS $ rdrNameOcc $ unLoc name) (mkFastString "|v")) fs) post
editSig t = t

editSynQuote :: TyClDecl RdrName -> TyClDecl RdrName
editSynQuote (SynDecl name tvs (L l (HsQuasiQuoteTy (HsQuasiQuote id pos fs))) fvs) =
  SynDecl name tvs (L l $ HsQuasiQuoteTy $ HsQuasiQuote id pos $ appendFS (appendFS (occNameFS $ rdrNameOcc $ unLoc name) (mkFastString "|t")) fs) fvs
editSynQuote t = t

--------------------------------------------------------------------------------

buildVarMap :: TypecheckedSource -> M.IntMap Id
buildVarMap = everything mappend (mkQ mempty ofId)
  where
    ofId id = M.singleton (getKey $ getUnique id) id

--------------------------------------------------------------------------------

pshow :: (Outputable a, GhcMonad m) => a -> m String
pshow x = do
  flags <- getSessionDynFlags
  return $ showPpr flags x

