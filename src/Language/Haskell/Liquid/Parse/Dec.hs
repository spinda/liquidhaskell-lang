module Language.Haskell.Liquid.Parse.Dec (
    decP
  ) where

import Control.Monad
import Control.Monad.Trans

import Data.Maybe

import Language.Haskell.TH.Syntax hiding (lift)

import Text.Parsec
import Text.Parsec.Combinator

import Language.Haskell.Liquid.Build
import Language.Haskell.Liquid.Util

import Language.Haskell.Liquid.Parse.Base
import Language.Haskell.Liquid.Parse.Type

--------------------------------------------------------------------------------
-- Top-Level Entry Point -------------------------------------------------------
--------------------------------------------------------------------------------

decP :: Parser [Dec]
decP = embedP <|> inlineP <|> tySynP <|> fnSigP

--------------------------------------------------------------------------------
-- FTycon Embed Annotations ----------------------------------------------------
--------------------------------------------------------------------------------

embedP :: Parser [Dec]
embedP = do
  simplified <- getSimplified
  tc         <- reserved "embed" *> (parens tyConP <|> tyConP)
  fc         <- reserved "as"    *> fTyConP
  tc'        <- lift $ fromMaybe (mkName $ tc_id tc) <$> lookupTypeName (tc_id tc)
  return $ if simplified
    then []
    else [annEmbedAs tc' fc]

--------------------------------------------------------------------------------
-- Inline Annotations ----------------------------------------------------------
--------------------------------------------------------------------------------

inlineP :: Parser [Dec]
inlineP = do
  simplified <- getSimplified
  var        <- mkName <$> varidP
  sig        <- option [] $ fnSigP' var
  return $ if simplified
    then sig
    else [annIsInline var] ++ sig

--------------------------------------------------------------------------------
-- Type Synonym Declarations ---------------------------------------------------
--------------------------------------------------------------------------------

tySynP :: Parser [Dec]
tySynP = named "type synonym" $ do
  simplified <- getSimplified
  con        <- (lift . newName) =<< reserved "type" *> conidP
  tvs        <- map (PlainTV . mkName) <$> many tyVarP
  evs        <- exprParamsP
  (_, ty)    <- reservedOp "=" *> typeP
  let tySynD = TySynD con tvs ty
  return $ if null evs || simplified
    then [tySynD]
    else [tySynD, annExprParams con evs]

exprParamsP :: Parser [String]
exprParamsP = do
  evs <- foldM checkUnique [] =<< many (withPos exprParamP)
  p   <- getPosition
  optional (tyVarP *> raiseErrAt p errExprParamTail)
  addExprParams evs
  return evs
  where
    checkUnique seen (p, param)
      | param `elem` seen = raiseErrAt p $ errDupExprParam param
      | otherwise         = return (param:seen)

--------------------------------------------------------------------------------
-- Function Signature Declarations ---------------------------------------------
--------------------------------------------------------------------------------

fnSigP :: Parser [Dec]
fnSigP = named "signature" $ fnSigP' =<< (mkName <$> varidP)

fnSigP' :: Name -> Parser [Dec]
fnSigP' var = named "signature" $
  reservedOp "::" *> fmap (return . SigD var . uncurry quantifyTy) typeP

--------------------------------------------------------------------------------
-- Error Messages --------------------------------------------------------------
--------------------------------------------------------------------------------

errExprParamTail :: String
errExprParamTail =
  "Type variables cannot follow expression parameters: expression parameters must be at the end"

errDupExprParam :: String -> String
errDupExprParam param =
  "Duplicate expression parameter \"" ++ param ++ "\""

