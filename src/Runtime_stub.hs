module Runtime (addLqAnnotation) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import RType

addLqAnnotation :: AnnTarget -> AnnType -> Q ()
addLqAnnotation _ _ = return ()

