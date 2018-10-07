{-# LANGUAGE OverloadedStrings #-}

module Types.Type where

import           Data.List   (foldl')
import qualified Data.Set    as Set
import           Data.String

import           Name
import           Types.Kind

data Type
  = TVar TVar                  -- type variable
  | TCon TyCon                 -- constant
  | TApp Type Type             -- application
  | TArr Type Type             -- arrow
  deriving (Eq, Ord, Show)

instance Named Type where
  getName (TVar t) = tvName t
  getName (TCon const) = case const of
    AlgTyCon n  -> n
    PrimTyCon n -> n

instance Named TVar where
  getName (TV n) = n

-- type variables

data TVar = TV
  { tvName :: Name
  } deriving (Eq, Ord, Show)

instance IsString TVar where
  fromString x = TV (fromString x)

-- type constructors

data TyCon
  = AlgTyCon { tyId :: Name }
  | PrimTyCon { tyId :: Name }
  deriving (Eq, Ord, Show)

instance IsString TyCon where
  fromString = AlgTyCon . fromString

-- alpha equivalence

class Alpha a where
  aeq :: a -> a -> Bool

instance Alpha TVar where
  aeq _ _ = True

instance Alpha Type where
  aeq (TVar _) (TVar _)     = True
  aeq (TApp a b) (TApp c d) = aeq a c && aeq b d
  aeq (TArr a b) (TArr c d) = aeq a c && aeq b d
  aeq (TCon a) (TCon b)     = a == b
  aeq _ _                   = False

instance Alpha Kind where
  aeq KStar KStar           = True
  -- aeq KPrim KPrim = True
  aeq (KArr a b) (KArr c d) = aeq a c && aeq b d
  aeq _ _                   = False

-- deconstructors

viewTArr :: Type -> [Type]
viewTArr (TArr t1 t2) = t1 : viewTArr t2
viewTArr t            = [t]

viewTApp :: Type -> [Type]
viewTApp t = go t []
  where
    go (TApp t1 t2) acc = go t1 (t2:acc)
    go t1 acc           = (t1 : acc)

typeArity :: Type -> Int
typeArity ty = length $ viewTArr ty

-- constructors

mkTCon :: Name -> Type
mkTCon n = TCon $ AlgTyCon n

mkTArr :: [Type] -> Type
mkTArr []     = error "not defined for empty lists"
mkTArr [t]    = t
mkTArr (t:ts) = TArr t (mkTArr ts)

mkTArr2 :: Type -> Type -> Type
mkTArr2 t1 t2 = mkTArr [t1, t2]

mkTApp :: TyCon -> [Type] -> Type
mkTApp tcon args = foldl' TApp (TCon tcon) args

mkTPair :: [Type] -> Type
mkTPair = foldr1 pair
  where pair x y = mkTApp (AlgTyCon "Pair") [x, y]

mkTList :: Type -> Type
mkTList t = TApp (TCon (AlgTyCon "List")) t

var :: Name -> Type
var = TVar . TV

-- built in types

tyInt :: Type
tyInt = TCon intTyCon

tyDouble :: Type
tyDouble = TCon doubleTyCon

tyChar :: Type
tyChar = TCon charTyCon

tyBool :: Type
tyBool = TCon (AlgTyCon "Bool")

tyList :: Type
tyList = TCon listTyCon

tyPair :: Type
tyPair = TCon pairTyCon

tyUnit :: Type
tyUnit = TCon unitTyCon

tyString :: Type
tyString = TCon (AlgTyCon "String")

tyPrims :: [Type]
tyPrims =
  [ tyInt
  , tyDouble
  , tyChar
  , tyBool
  , tyUnit
  ]

intTyCon :: TyCon
intTyCon = PrimTyCon "Int"

doubleTyCon :: TyCon
doubleTyCon = PrimTyCon "Double"

charTyCon :: TyCon
charTyCon = PrimTyCon "Char"

listTyCon :: TyCon
listTyCon = AlgTyCon "List"

pairTyCon :: TyCon
pairTyCon = AlgTyCon "Pair"

unitTyCon :: TyCon
unitTyCon = AlgTyCon "()"

tyArrow :: Type
tyArrow = TCon (AlgTyCon "->")

-- Variables

instance FreeVars Type where
  freeVars ty = case ty of
    TVar (TV n) -> Set.singleton n
    TCon _ -> Set.empty
    TApp t1 t2 -> freeVars t1 `Set.union` freeVars t2
    TArr t1 t2 -> freeVars t1 `Set.union` freeVars t2

-- class HasKind t where
--   kind :: t -> Kind

-- instance HasKind Tyvar where
--   kind (TV _ k) = k
-- instance HasKind Tycon where
--   kind (TC _ k) = k
-- instance HasKind Type where
--   kind (TVar u) = kind u
--   kind (TCon c) = kind c
--   kind (TApp t _) = case kind t of
--                       (KArr _ k) -> k
--                       k          -> k
--   kind (TArr t _) = kind t
