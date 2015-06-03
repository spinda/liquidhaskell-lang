instance Hashable Name where
  hashWithSalt = uniqueHash

uniqueHash :: Uniquable a => Int -> a -> Int
uniqueHash i = hashWithSalt i . getKey . getUnique

--------------------------------------------------------------------------------

{-

type Unify = State UnifyState

data UnifyState =
  US { us_anns :: M.HashMap Name AnnType
     , us_tenv :: TyVarEnv SpecType
     }

unifyType :: Type -> AnnType -> SpecType
unifyType ty at = runState (unifyType ty at) (US mempty emptyVarEnv)

unifyType' :: Type -> AnnType -> Unify SpecType

unifyType' (TyConApp tc as) (RApp _ as' r) = do
  as'' <- zipWithM unifyType' as as'
  case tcExpandTyCon_maybe tc tys of
    Just (TvSubst _ tenv, rhs, tys') -> do
      let 
    _ ->
      return $ RApp tc as'' r



  | Just (tenv, rhs, tys') <- tcExpandTyCon_maybe tc tys = 
    
  | otherwise
    = RApp

unifyType' (AppTy t1 t2) (RAppTy t1' t2' r) =
  RAppTy <$> unifyType' t1 t1' <*> unifyType' t2 t2' <*> pure r

unifyType' (FunTy i o) (RFun b i' o' r) =
  RFun b <$> unifyType' i i' <*> unifyType' o o' <*> pure r

unifyType' (ForAllTy tv ty) (RAllT _ ty') = -- TODO
  RAllT tv <$> unifyType' ty ty'

unifyType' (LitTy _) _ =
  error "TODO: LiquidHaskell doesn't support type-level literals, yet"



resolveType' scope (TyConApp tc as) (RApp _ as' r)
  | Just (tenv, rhs, tys') <- tcExpandTyCon_maybe tc tys
  = resolveType' (mkAppTys (substTy (mkTopTvSubst tenv) rhs) tys') (mkRAppTys $ lookup (yConName)
  | otherwidse
  = RApp tc (zipWith resolveType' as as') r

resolveType' _ (TyVarTy tv) (RVar _ r) = 
  RVar tv r
resolveType' (FunTy i o) (RFun b i' o' r) =
  RFun b (resolveType' scope i i') (resolveType' scope o o') r

--------------------------------------------------------------------------------

