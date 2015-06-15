{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main (
    main
  ) where

import GHC
import GHC.Paths

import Digraph
import DynFlags
import HsImpExp
import MonadUtils
import Outputable
import Var

import Data.Generics.Aliases
import Data.Generics.Schemes

import System.Environment

import Text.PrettyPrint.GenericPretty
import Text.PrettyPrint.HughesPJ

import Language.Haskell.Liquid.Extract
import Language.Haskell.Liquid.Types
import Language.Haskell.Liquid.WiredIns

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
            [ ExposePackage (PackageArg "liquidhaskell-lang") (ModRenaming True [])
            ]
        }

      targetArgs <- liftIO getArgs
      targets    <- mapM (`guessTarget` Nothing) targetArgs
      setTargets targets

      graph <- depanal [] True

      setContext [IIDecl $ simpleImportDecl $ mkModuleName "Language.Haskell.Liquid.RType"]
      typeData <- runWiredM $ concatMapM extractTypeData $ flattenSCCs $ topSortModuleGraph False graph Nothing

      liftIO $ putStrLn "types..."
      mapM_ printTypeData typeData
  where
    printTypeData (var, ty) = do
      var' <- pshow var
      liftIO $ do
        putStrLn $ "=== " ++ var' ++ " ==="
        putStrLn $ render $ doc ty

--------------------------------------------------------------------------------
-- Type Data Extraction --------------------------------------------------------
--------------------------------------------------------------------------------

extractTypeData :: GhcMonad m => ModSummary -> WiredM m [(Var, SpecType)]
extractTypeData summ = do
  parsed  <- parseModule summ
  typecheckModule parsed

  let parsed' = parsed { pm_mod_summary = (pm_mod_summary parsed) { ms_textual_imps = rewrite $ ms_textual_imps $ pm_mod_summary parsed
                                                                  }
                       , pm_parsed_source = rewrite' $ pm_parsed_source parsed
                       }
  typechecked <- typecheckModule parsed'
  loadModule typechecked

  setContext [IIModule $ moduleName $ ms_mod summ]
  extractTopSigs typechecked
  where
    rewrite = everywhere (mkT $ replaceModule (mkModuleName "LiquidHaskell") (mkModuleName "LiquidHaskell_"))
    rewrite' = everywhere (mkT $ replaceModule (mkModuleName "LiquidHaskell") (mkModuleName "LiquidHaskell_"))

replaceModule :: ModuleName -> ModuleName -> ModuleName -> ModuleName
replaceModule orig repl mod
  | mod == orig = repl
  | otherwise   = mod

--------------------------------------------------------------------------------
-- Utility Functions -----------------------------------------------------------
--------------------------------------------------------------------------------

pshow :: (Outputable a, GhcMonad m) => a -> m String
pshow x = do
  flags <- getSessionDynFlags
  return $ showPpr flags x

