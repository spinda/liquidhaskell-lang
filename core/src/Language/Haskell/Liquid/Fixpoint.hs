{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Language.Haskell.Liquid.Fixpoint where

-- TODO: Messy, temporary copy of parts of the Fixpoint types with String,
--       until Symbol/InternedText refreshing is implemented

import           Control.Arrow              (first)

import           Data.Data                  hiding (TyCon)
import           Data.List
import           Data.Maybe
import           Data.String
import           Data.Typeable              hiding (TyCon)

import qualified Data.HashMap.Strict        as M

import           GHC.Generics

import qualified Language.Haskell.TH.Syntax as TH

import           Text.PrettyPrint.HughesPJ  hiding (first)
import           Text.Printf

data Located a = Loc { loc  :: !TH.Loc
                     , val  :: a
                     } deriving (Data, Typeable, Generic)

data Pred = PTrue
          | PFalse
          | PAnd   ![Pred]
          | POr    ![Pred]
          | PNot   !Pred
          | PImp   !Pred !Pred
          | PIff   !Pred !Pred
          | PBexp  !Expr
          | PAtom  !Brel  !Expr !Expr
          | PKVar  !KVar !Subst
          | PAll   ![(String, Sort)] !Pred
          | PTop
          deriving (Eq, Ord, Show, Data, Typeable, Generic)

type LocSymbol = Located String

newtype KVar = KV {kv :: String } deriving (Eq, Ord, Data, Typeable, Generic, IsString)

data Expr = ESym !SymConst
          | ECon !Constant
          | EVar !String
          | ELit !LocSymbol !Sort
          | EApp !LocSymbol ![Expr]
          | ENeg !Expr
          | EBin !Bop !Expr !Expr
          | EIte !Pred !Expr !Expr
          | ECst !Expr !Sort
          | EBot
          deriving (Eq, Ord, Show, Data, Typeable, Generic)

newtype Refa = Refa { raPred :: Pred }
               deriving (Eq, Ord, Show, Data, Typeable, Generic)

newtype Reft = Reft (String, Refa)
               deriving (Eq, Ord, Data, Typeable, Generic)

data Sort = FInt
          | FReal
          | FNum                 -- ^ numeric kind for Num tyvars
          | FFrac                -- ^ numeric kind for Fractional tyvars
          | FObj  String         -- ^ uninterpreted type
          | FVar  !Int           -- ^ fixpoint type variable
          | FFunc !Int ![Sort]   -- ^ type-var arity, in-ts ++ [out-t]
          | FApp FTycon [Sort]   -- ^ constructed type
              deriving (Eq, Ord, Show, Data, Typeable, Generic)

data SymConst = SL !String
              deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Constant = I !Integer
              | R !Double
              | L !String !Sort
              deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Brel = Eq | Ne | Gt | Ge | Lt | Le | Ueq | Une
            deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Bop  = Plus | Minus | Times | Div | Mod
            deriving (Eq, Ord, Show, Data, Typeable, Generic)
              -- NOTE: For "Mod" 2nd expr should be a constant or a var *)

pAnd          = simplify . PAnd

class Fixpoint a where
  toFix    :: a -> Doc

  simplify :: a -> a
  simplify =  id

instance Fixpoint a => Fixpoint (Maybe a) where
  toFix    = maybe (text "Nothing") ((text "Just" <+>) . toFix)
  simplify = fmap simplify

instance Fixpoint a => Fixpoint [a] where
  toFix xs = brackets $ sep $ punctuate (text ";") (fmap toFix xs)
  simplify = map simplify

instance (Fixpoint a, Fixpoint b) => Fixpoint (a,b) where
  toFix   (x,y)  = toFix x <+> text ":" <+> toFix y
  simplify (x,y) = (simplify x, simplify y)


instance Fixpoint Pred where
  toFix PTop             = text "???"
  toFix PTrue            = text "true"
  toFix PFalse           = text "false"
  toFix (PBexp e)        = parens $ text "?" <+> toFix e
  toFix (PNot p)         = parens $ text "~" <+> parens (toFix p)
  toFix (PImp p1 p2)     = parens $ toFix p1 <+> text "=>" <+> toFix p2
  toFix (PIff p1 p2)     = parens $ toFix p1 <+> text "<=>" <+> toFix p2
  toFix (PAnd ps)        = text "&&" <+> toFix ps
  toFix (POr  ps)        = text "||" <+> toFix ps
  toFix (PAtom r e1 e2)  = parens $ toFix e1 <+> toFix r <+> toFix e2
  toFix (PKVar k su)     = toFix k <> toFix su
  toFix (PAll xts p)     = text "forall" <+> toFix (map (first Shim) xts) <+> text "." <+> toFix p

  simplify (PAnd [])     = PTrue
  simplify (POr  [])     = PFalse
  simplify (PAnd [p])    = simplify p
  simplify (POr  [p])    = simplify p

  simplify (PAnd ps)
    | any isContraPred ps = PFalse
    | otherwise           = PAnd $ filter (not . isTautoPred) $ map simplify ps

  simplify (POr  ps)
    | any isTautoPred ps = PTrue
    | otherwise          = POr  $ filter (not . isContraPred) $ map simplify ps

  simplify p
    | isContraPred p     = PFalse
    | isTautoPred  p     = PTrue
    | otherwise          = p


isContraPred   :: Pred -> Bool
isContraPred z = eqC z || (z `elem` contras)
  where
    contras    = [PFalse]

    eqC (PAtom Eq (ECon x) (ECon y))
               = x /= y
    eqC (PAtom Ueq (ECon x) (ECon y))
               = x /= y
    eqC (PAtom Ne x y)
               = x == y
    eqC (PAtom Une x y)
               = x == y
    eqC _      = False

isTautoPred   :: Pred -> Bool
isTautoPred z  = z == PTop || z == PTrue || eqT z
  where
    eqT (PAtom Le x y)
               = x == y
    eqT (PAtom Ge x y)
               = x == y
    eqT (PAtom Eq x y)
               = x == y
    eqT (PAtom Ueq x y)
               = x == y
    eqT (PAtom Ne (ECon x) (ECon y))
               = x /= y
    eqT (PAtom Une (ECon x) (ECon y))
               = x /= y
    eqT _      = False

instance Monoid Pred where
  mempty      = PTrue
  mappend p q = pAnd [p, q]
  mconcat     = pAnd

instance Monoid Reft where
  mempty  = trueReft
  mappend = meetReft

instance Monoid Refa where
  mempty          = Refa mempty
  mappend ra1 ra2 = Refa $ mappend (raPred ra1) (raPred ra2)

meetReft (Reft (v, ra)) (Reft (v', ra'))
  | v == v'          = Reft (v , ra  `mappend` ra')
  | v == dummySymbol = Reft (v', ra' `mappend` (ra `subst1`  (v , EVar v')))
  | otherwise        = Reft (v , ra  `mappend` (ra' `subst1` (v', EVar v )))

dummySymbol :: String
dummySymbol = "_LIQUID_dummy"

-------------------------------------------------------
------------------- Substitutions ---------------------
-------------------------------------------------------

class Subable a where
  syms   :: a -> [String]
  substa :: (String -> String) -> a -> a
  -- substa f  = substf (EVar . f)

  substf :: (String -> Expr) -> a -> a
  subst  :: Subst -> a -> a
  subst1 :: a -> (String, Expr) -> a
  -- subst1 y (x, e) = subst (Su $ M.singleton x e) y
  subst1 y (x, e) = subst (Su [(x,e)]) y

subst1Except :: (Subable a) => [String] -> a -> (String, Expr) -> a
subst1Except xs z su@(x, _)
  | x `elem` xs = z
  | otherwise   = subst1 z su

substfExcept :: (String -> Expr) -> [String] -> String -> Expr
substfExcept f xs y = if y `elem` xs then EVar y else f y

substExcept  :: Subst -> [String] -> Subst
-- substExcept  (Su m) xs = Su (foldr M.delete m xs)
substExcept  (Su xes) xs = Su $ filter (not . (`elem` xs) . fst) xes

instance Subable String where
  substa f                 = f
  substf f x               = subSymbol (Just (f x)) x
  subst su x               = subSymbol (Just $ appSubst su x) x -- subSymbol (M.lookup x s) x
  syms x                   = [x]

subSymbol (Just (EVar y)) _ = y
subSymbol Nothing         x = x
subSymbol a               b = error (printf "Cannot substitute symbol %s with expression %s" b (showFix a))

instance Subable Expr where
  syms                     = exprSymbols
  substa f                 = substf (EVar . f)
  substf f (EApp s es)     = EApp (substf f s) $ map (substf f) es
  substf f (ENeg e)        = ENeg (substf f e)
  substf f (EBin op e1 e2) = EBin op (substf f e1) (substf f e2)
  substf f (EIte p e1 e2)  = EIte (substf f p) (substf f e1) (substf f e2)
  substf f (ECst e so)     = ECst (substf f e) so
  substf f e@(EVar x)      = f x
  substf _ e               = e

  subst su (EApp f es)     = EApp (subst su f) $ map (subst su) es
  subst su (ENeg e)        = ENeg (subst su e)
  subst su (EBin op e1 e2) = EBin op (subst su e1) (subst su e2)
  subst su (EIte p e1 e2)  = EIte (subst su p) (subst su e1) (subst su e2)
  subst su (ECst e so)     = ECst (subst su e) so
  subst su (EVar x)        = appSubst su x
  subst _ e                = e


instance Subable Pred where
  syms                     = predSymbols
  substa f                 = substf (EVar . f)
  substf f (PAnd ps)       = PAnd $ map (substf f) ps
  substf f (POr  ps)       = POr  $ map (substf f) ps
  substf f (PNot p)        = PNot $ substf f p
  substf f (PImp p1 p2)    = PImp (substf f p1) (substf f p2)
  substf f (PIff p1 p2)    = PIff (substf f p1) (substf f p2)
  substf f (PBexp e)       = PBexp $ substf f e
  substf f (PAtom r e1 e2) = PAtom r (substf f e1) (substf f e2)
  substf _ p@(PKVar _ _)   = p
  substf _  (PAll _ _)     = error "substf: FORALL"
  substf _  p              = p

  subst su (PAnd ps)       = PAnd $ map (subst su) ps
  subst su (POr  ps)       = POr  $ map (subst su) ps
  subst su (PNot p)        = PNot $ subst su p
  subst su (PImp p1 p2)    = PImp (subst su p1) (subst su p2)
  subst su (PIff p1 p2)    = PIff (subst su p1) (subst su p2)
  subst su (PBexp e)       = PBexp $ subst su e
  subst su (PAtom r e1 e2) = PAtom r (subst su e1) (subst su e2)
  subst su (PKVar k su')   = PKVar k $ su' `catSubst` su
  subst _  (PAll _ _)      = error "subst: FORALL"
  subst _  p               = p

instance Subable Refa where
  syms (Refa p)           = syms p
  -- syms (RKvar k (Su su'))  = k : concatMap syms ({- M.elems -} su')
  subst su (Refa p)       = Refa $ subst su p
  substa f                 = substf (EVar . f)
  substf f (Refa p)       = Refa (substf f p)

instance (Subable a, Subable b) => Subable (a,b) where
  syms  (x, y)   = syms x ++ syms y
  subst su (x,y) = (subst su x, subst su y)
  substf f (x,y) = (substf f x, substf f y)
  substa f (x,y) = (substa f x, substa f y)

instance Subable Reft where
  syms (Reft (v, ras))      = v : syms ras
  substa f (Reft (v, ras))  = Reft (f v, substa f ras)
  subst su (Reft (v, ras))  = Reft (v, subst (substExcept su [v]) ras)
  substf f (Reft (v, ras))  = Reft (v, substf (substfExcept f [v]) ras)
  subst1 (Reft (v, ras)) su = Reft (v, subst1Except [v] ras su)


instance Subable SortedReft where
  syms               = syms . sr_reft
  subst su (RR so r) = RR so $ subst su r
  substf f (RR so r) = RR so $ substf f r
  substa f (RR so r) = RR so $ substa f r

newtype Subst = Su [(String, Expr)]
                deriving (Eq, Ord, Data, Typeable, Generic)

appSubst :: Subst -> String -> Expr
appSubst (Su s) x        = fromMaybe (EVar x) (lookup x s)

emptySubst :: Subst
emptySubst = Su [] -- M.empty

catSubst :: Subst -> Subst -> Subst
catSubst = unsafeCatSubst

mkSubst  :: [(String, Expr)] -> Subst
mkSubst  = unsafeMkSubst

isEmptySubst :: Subst -> Bool
isEmptySubst (Su xes) = null xes

unsafeMkSubst  = Su

unsafeCatSubst (Su s1) θ2@(Su s2) = Su $ s1' ++ s2
  where
    s1'                           = mapSnd (subst θ2) <$> s1

newtype FTycon = TC LocSymbol deriving (Eq, Ord, Show, Data, Typeable, Generic)

trueReft  = Reft (vv_, trueRefa)
falseReft = Reft (vv_, Refa PFalse)

trueRefa  :: Refa
trueRefa  = Refa PTrue

showFix :: (Fixpoint a) => a -> String
showFix =  render . toFix

exprSymbols :: Expr -> [String]
exprSymbols = go
  where
    go (EVar x)        = [x]
    go (ELit x _)      = [val x]
    go (EApp f es)     = val f : concatMap go es
    go (ENeg e)        = go e
    go (EBin _ e1 e2)  = go e1 ++ go e2
    go (EIte p e1 e2)  = predSymbols p ++ go e1 ++ go e2
    go (ECst e _)      = go e
    go _               = []

predSymbols :: Pred -> [String]
predSymbols = go
  where
    go (PAnd ps)          = concatMap go ps
    go (POr ps)           = concatMap go ps
    go (PNot p)           = go p
    go (PIff p1 p2)       = go p1 ++ go p2
    go (PImp p1 p2)       = go p1 ++ go p2
    go (PBexp e)          = exprSymbols e
    go (PAtom _ e1 e2)    = exprSymbols e1 ++ exprSymbols e2
    go (PKVar _ (Su su')) = {- CUTSOLVER k : -} concatMap syms su'
    go (PAll xts p)       = (fst <$> xts) ++ go p
    go _                  = []

data SortedReft = RR { sr_sort :: !Sort, sr_reft :: !Reft }
                  deriving (Eq, Show, Data, Typeable, Generic)

mapSnd f (x, y)  = (x, f y)

vv_ :: String
vv_ = "VV"

instance Show Subst where
  show = showFix

instance Fixpoint Subst where
  toFix (Su m) = case {- hashMapToAscList -} m of
                   []  -> empty
                   xys -> hcat $ map (\(x,y) -> brackets $ toFix (Shim x) <> text ":=" <> toFix y) xys

instance Show KVar where
  show (KV x) = "$" ++ show x

instance Show a => Show (Located a) where
  show (Loc l x) = show x ++ " defined at: " ++ show l

instance Eq a => Eq (Located a) where
  (Loc _ x) == (Loc _ y) = x == y

instance Ord a => Ord (Located a) where
  compare x y = compare (val x) (val y)

instance Subable a => Subable (Located a) where
  syms (Loc _ x)   = syms x
  substa f (Loc l  x) = Loc l (substa f x)
  substf f (Loc l  x) = Loc l (substf f x)
  subst su (Loc l  x) = Loc l (subst su x)

instance Show Reft where
  show (Reft (x, r)) = render $ toFix (Shim x, r)


instance Fixpoint Sort where
  toFix = toFixSort

newtype Shim = Shim String

instance Fixpoint Shim where
  toFix (Shim s) = text s

toFixSort :: Sort -> Doc
toFixSort (FVar i)        = text "@"   <> parens (toFix i)
toFixSort FInt            = text "int"
toFixSort FReal           = text "real"
toFixSort FFrac           = text "frac"
toFixSort (FObj x)        = toFix (Shim x)
toFixSort FNum            = text "num"
toFixSort (FFunc n ts)    = text "func" <> parens (toFix n <> text ", " <> toFix ts)
toFixSort (FApp c [t])
  | isListTC c            = brackets $ toFixSort t
toFixSort (FApp c [FApp c' [],t])
  | isFAppTyTC c &&
    isListTC c'           = brackets $ toFixSort t
toFixSort (FApp c ts)     = toFix c <+> hcat (intersperse space (fp <$> ts))
    where
      fp s@(FApp _ (_:_)) = parens $ toFixSort s
      fp s                = toFixSort s

isListTC, isFAppTyTC :: FTycon -> Bool
isListTC (TC (Loc _ c)) = c == listConName
isTupTC  (TC (Loc _ c)) = c == tupConName
isFAppTyTC = (== appFTyCon)

intFTyCon, boolFTyCon, realFTyCon, strFTyCon, propFTyCon, appFTyCon, listFTyCon :: FTycon
intFTyCon  = TC $ dummyLoc "int"
boolFTyCon = TC $ dummyLoc "bool"
realFTyCon = TC $ dummyLoc "real"
strFTyCon  = TC $ dummyLoc strConName
propFTyCon = TC $ dummyLoc propConName
listFTyCon = TC $ dummyLoc listConName
appFTyCon  = TC $ dummyLoc "FAppTy"

preludeName, dummyName, boolConName, funConName, listConName, tupConName, propConName, strConName, vvName :: String
preludeName  = "Prelude"
dummyName    = "_LIQUID_dummy"
boolConName  = "Bool"
funConName   = "->"
listConName  = "[]" -- "List"
tupConName   = "()" -- "Tuple"
propConName  = "Prop"
hpropConName = "HProp"
strConName   = "Str"
vvName       = "VV"
symSepName   = '#'

nilName      = "nil"    :: String
consName     = "cons"   :: String

size32Name   = "Size32" :: String
size64Name   = "Size64" :: String
bitVecName   = "BitVec" :: String
bvOrName     = "bvor"   :: String
bvAndName    = "bvAnd"  :: String

dummyLoc :: a -> Located a
dummyLoc = Loc l where l = dummyPos "Fixpoint.Types.dummyLoc"

dummyPos   :: String -> TH.Loc
dummyPos s = TH.Loc s s s (0, 0) (0, 0)

instance Fixpoint Expr where
  toFix (ESym c)       = toFix $ Shim $ encodeSymConst c
  toFix (ECon c)       = toFix c
  toFix (EVar s)       = toFix $ Shim s
  toFix (ELit (Loc l s) _)     = toFix $ Loc l $ Shim s
  toFix (EApp (Loc l s) es)    = toFix (Loc l (Shim s)) <> parens (toFix es)
  toFix (ENeg e)       = parens $ text "-" <+> parens (toFix e)
  toFix (EBin o e1 e2) = parens $ toFix e1 <+> toFix o <+> toFix e2
  toFix (EIte p e1 e2) = parens $ toFix p <+> text "?" <+> toFix e1 <+> text ":" <+> toFix e2
  toFix (ECst e so)    = parens $ toFix e <+> text " : " <+> toFix so
  toFix (EBot)         = text "_|_"

encodeSymConst        :: SymConst -> String
encodeSymConst (SL s) = litPrefix `mappend` s

litPrefix    :: String
litPrefix    = "lit" ++ [symSepName]

instance Fixpoint Brel where
  toFix Eq  = text "="
  toFix Ne  = text "!="
  toFix Ueq = text "~~"
  toFix Une = text "!~"
  toFix Gt  = text ">"
  toFix Ge  = text ">="
  toFix Lt  = text "<"
  toFix Le  = text "<="

instance Fixpoint Bop where
  toFix Plus  = text "+"
  toFix Minus = text "-"
  toFix Times = text "*"
  toFix Div   = text "/"
  toFix Mod   = text "mod"

instance Fixpoint KVar where
  toFix (KV k) = text "$" <> toFix (Shim k)

instance Fixpoint Refa where
  toFix (Refa p)     = toFix $ conjuncts p
  -- toFix (RPvar p)    = toFix p

conjuncts (PAnd ps) = concatMap conjuncts ps
conjuncts p
  | isTautoPred p   = []
  | otherwise       = [p]

instance Fixpoint Int where
  toFix = tshow

instance Fixpoint FTycon where
  toFix (TC (Loc l s))       = toFix (Loc l (Shim s))

instance Fixpoint Integer where
  toFix = integer

instance Fixpoint Double where
  toFix = double

instance Fixpoint Constant where
  toFix (I i)   = toFix i
  toFix (R i)   = toFix i
  toFix (L s t) = parens $ text "lit" <+> text "\"" <> toFix (Shim s) <> text "\"" <+> toFix t

tshow              = text . show

instance Fixpoint a => Fixpoint (Located a) where
  toFix = toFix . val

instance Fixpoint Reft where
  toFix = pprReftPred

pprReftPred (Reft (_, Refa p))
  | isTautoPred p
  = text "true"
  | otherwise
  = ppRas [Refa p]

ppRas = cat . punctuate comma . map toFix . flattenRefas

flattenRefas ::  [Refa] -> [Refa]
flattenRefas         = concatMap flatRa
  where
    flatRa (Refa p)  = Refa <$> flatP p
    flatP  (PAnd ps) = concatMap flatP ps
    flatP  p         = [p]

prims :: [String]
prims = [ propConName
        , hpropConName
        , vvName
        , "Pred"
        , "List"
        , "Set_Set"
        , "Set_sng"
        , "Set_cup"
        , "Set_cap"
        , "Set_dif"
        , "Set_emp"
        , "Set_empty"
        , "Set_mem"
        , "Set_sub"
        , "Map_t"
        , "Map_select"
        , "Map_store"
        , size32Name
        , size64Name
        , bitVecName
        , bvOrName
        , bvAndName
        , "FAppTy"
        , nilName
        , consName
        ]

