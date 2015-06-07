{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main (
    main
  ) where

-- TODO: Split up this module, move parts to liquidhaskell-ghc

import           GHC
import           GHC.Desugar
import           GHC.Paths                              (libdir)

import           Annotations
import           Bag
import           Convert
import           CoreMonad
import           Digraph
import           DynFlags
import           FastString
import           GhcMonad
import           HscMain
import           HscTypes
import           MonadUtils
import           Name
import           OccName
import           Outputable
import           RdrName
import           Serialized
import           TcRnDriver
import           TyCon
import           Type
import           UniqFM
import           Unique
import           Var

import           HsBinds
import           HsDecls
import           HsTypes

import           Control.Monad

import           Data.Data
import           Data.Maybe
import           Data.Typeable
import           Data.Word

import qualified Data.IntMap.Strict                     as M

import           Data.Generics.Aliases
import           Data.Generics.Schemes
import           Data.Generics.Text

import           System.Environment

import           Text.PrettyPrint.GenericPretty
import           Text.PrettyPrint.HughesPJ

import           Language.Haskell.Liquid.Expand
import           Language.Haskell.Liquid.RType
import           Language.Haskell.Liquid.SpecType

import           Language.Haskell.Liquid.Quasi.Metadata

--------------------------------------------------------------------------------
-- Top-Level Entry Point -------------------------------------------------------
--------------------------------------------------------------------------------

main :: IO ()
main =
  defaultErrorHandler defaultFatalMessager defaultFlushOut $
    runGhc (Just libdir) $ do
      dflags <- getSessionDynFlags
      setSessionDynFlags $ dflags
        { ghcLink      = LinkInMemory
        , ghcMode      = CompManager
        , hscTarget    = HscInterpreted
        , packageFlags = packageFlags dflags ++
            [ HidePackage "liquidhaskell-lang"
            , ExposePackage (PackageArg "liquidhaskell-frontend") (ModRenaming True [("Language.Haskell.Liquid.Quasi", "LiquidHaskell")])
            ]
        }

      targetArgs <- liftIO getArgs
      targets    <- mapM (`guessTarget` Nothing) targetArgs
      setTargets targets

      graph      <- depanal [] True
      typeData   <- concatMapM extractTypeData $ flattenSCCs $ topSortModuleGraph False graph Nothing

      let instantiated = map (\(name, ty, ann) -> (name, instantiateRTy ty ann)) typeData
      let expanded     = expandRTys instantiated

      liftIO $ mapM_ (putStrLn . render . doc) expanded

--------------------------------------------------------------------------------
-- Type Data Extraction --------------------------------------------------------
--------------------------------------------------------------------------------

extractTypeData :: GhcMonad m => ModSummary -> m [(Name, Type, AnnType)]
extractTypeData summ = do
  parsed      <- parseModule summ
  let parsed'  = parsed { pm_parsed_source = editQuasiQuotes $ pm_parsed_source parsed }
  typechecked <- typecheckModule parsed'
  desugared   <- desugarModule typechecked

  loadModule desugared
  setContext [IIModule $ moduleName $ ms_mod summ]

  let varMap = buildVarMap $ tm_typechecked_source typechecked

  let topLevelAnns = annotationsOfType $ dm_core_module desugared
  -- TODO: Only extract annotations on 'LiquidHaskell here
  let localAnns    = annotationsOfType $ dm_core_module desugared

  topLevel <- mapM extractTopLevel topLevelAnns
  local    <- mapM (extractLocal varMap . snd) localAnns
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
    ofThing (ATyCon tc) =
      fromMaybe (error "TODO: unexpected non-synonym ATyCon") (synTyConRhs_maybe tc)
    ofThing (ACoAxiom _) =
      error "TODO: unexpected ACoAxiom"

extractLocal :: GhcMonad m => M.IntMap Id -> LqLocal -> m (Name, Type, AnnType)
extractLocal varMap (LqLocal id ty) = do
  let Just var = M.lookup id varMap
  return (getName var, varType var, ty)

--------------------------------------------------------------------------------
-- Source Annotation Retrieval -------------------------------------------------
--------------------------------------------------------------------------------

annotationsOfType :: (Data a, Typeable a) => ModGuts -> [(Name, a)]
annotationsOfType = mapMaybe annotationOfType . mg_anns

annotationOfType :: (Data a, Typeable a) => Annotation -> Maybe (Name, a)
annotationOfType (Annotation (NamedTarget name) payload) =
  (name, ) <$> fromSerialized deserializeWithData payload
annotationOfType _ = Nothing

--------------------------------------------------------------------------------
-- `lq` QuasiQuote Pre-Processing ----------------------------------------------
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
editSigQuote t = t

editSynQuote :: TyClDecl RdrName -> TyClDecl RdrName
editSynQuote (SynDecl name tvs (L l (HsQuasiQuoteTy (HsQuasiQuote id pos fs))) fvs) =
  SynDecl name tvs (L l $ HsQuasiQuoteTy $ HsQuasiQuote id pos $ appendFS (appendFS (occNameFS $ rdrNameOcc $ unLoc name) (mkFastString "|t")) fs) fvs
editSynQuote t = t

--------------------------------------------------------------------------------
-- Extract All Vars from AST ---------------------------------------------------
--------------------------------------------------------------------------------

buildVarMap :: TypecheckedSource -> M.IntMap Id
buildVarMap = everything mappend (mkQ mempty ofId)
  where
    ofId id = M.singleton (getKey $ getUnique id) id

--------------------------------------------------------------------------------
-- GHC Utility Functions -------------------------------------------------------
--------------------------------------------------------------------------------

pshow :: (Outputable a, GhcMonad m) => a -> m String
pshow x = do
  flags <- getSessionDynFlags
  return $ showPpr flags x

