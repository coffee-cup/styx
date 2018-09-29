{-# LANGUAGE OverloadedStrings #-}

module Type where

import           Name

import           Data.String

data Type
  = TVar Tyvar     -- type variable
  | TCon Tycon     -- constant
  | TApp Type Type -- application
  | TArr Type Type -- arrow
  deriving (Eq, Ord, Show)

data Kind
  = KStar
  | KArr Kind Kind
  deriving (Eq, Ord, Show)

-- type variable
data Tyvar = TV Name
  deriving (Eq, Ord, Show)

-- type constant
data Tycon = TC Name
  deriving (Eq, Ord, Show)

-- qualified type
data Qual t = [Pred] :=> t
  deriving (Eq, Ord, Show)

-- predicate
data Pred = IsIn Name Type
  deriving (Eq, Ord, Show)

instance IsString Tyvar where
  fromString = TV . fromString

instance IsString Tycon where
  fromString = TC . fromString

instance Named Type where
  getName (TVar (TV n)) = n
  getName (TCon (TC n)) = n

class HasKind t where
  kind :: t -> Kind

instance HasKind Tyvar where
  kind (TV _) = KStar
instance HasKind Tycon where
  kind (TC _) = KStar
instance HasKind Type where
  kind (TVar u) = kind u
  kind (TCon c) = kind c
  kind (TApp t _) = case kind t of
                      (KArr _ k) -> k
                      k          -> k
  kind (TArr t _) = kind t
