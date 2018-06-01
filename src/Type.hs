{-# LANGUAGE OverloadedStrings #-}

module Type where

import           Name

import           Data.Char
import           Data.List   (foldl')
import           Data.String

data Type
  = TVar TVar -- type variable
  | TCon TyCon -- constant
  | TApp Type Type -- application
  | TArr Type Type -- arrow
  | TForall [Pred] [TVar] Type
  deriving (Eq, Ord, Show)

data Kind
  = KStar
  | KArr Kind Kind
  | KPrim
  | KVar Name
  deriving (Eq, Ord, Show)

data TyCon
  = AlgTyCon { tyId :: Name }
  | PrimTyCon { tyId :: Name}
  deriving (Eq, Ord, Show)

data Pred
  = IsIn Name Type
  deriving (Eq, Ord, Show)

-- Type Variables

data TVar = TV
  { tvName :: Name }
  deriving (Eq, Ord, Show)

instance IsString TVar where
  fromString x = TV (fromString x)

instance IsString TyCon where
  fromString = AlgTyCon . fromString
