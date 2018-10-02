{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Frontend where

import           Prelude      hiding (concatMap, foldr, foldr1)
import Data.List (foldl')

import           Name
import           Types.Pred
import           Types.Scheme
import           Types.Type

-- Constructor
type Constr = Name

-- Expressions
data Expr
  = EApp Expr Expr               -- a b
  | EInApp Expr Expr             -- a + b
  | EPreApp Expr Expr            -- !a
  | EVar Name                    -- x
  | ELam [Name] [Expr]           -- \x -> y
  | EAss Name Expr               -- x = a
  | ELit Literal                 -- 2, "hello"
  | EIf Expr [Expr] [Expr]       -- if x then tr else fl
  | EAnn Expr Type               -- (x : Int)
  | EParens Expr                 -- (a)
  deriving (Eq, Show)

-- Literals
data Literal
  = LitInt Integer               -- 1
  | LitDouble Double             -- 1.1
  | LitBool Bool                 -- true, false
  | LitChar Char                 -- 'a'
  | LitString String             -- "hello"
  deriving (Eq, Ord, Show)

-- BindGroups
data BindGroup = BindGroup
  { _matchName :: Name
  , _matchPats :: [Match]
  , _matchType :: Maybe Type
  } deriving (Eq, Show)

-- Matches
data Match = Match
  { _matchPat  :: [Pattern]
  , _matchBody :: [Expr]
  } deriving (Eq, Show)

-- Patterns
data Pattern
  = PVar Name                    -- x
  | PCon Constr [Pattern]        -- C x y
  | PLit Literal                 -- 3
  | PWild                        -- _
  deriving (Eq, Show)

-- data DataDecl
--   = ...

-- Declarations
data Decl
  = FunDecl BindGroup   -- f x = x + 1
  | TypeDecl Name Scheme  -- f :: Int -> Int
  | ClassDecl ClassDecl -- class (P) => T where { ... }
  | InstDecl InstDecl -- instance (P) => T where { ... }
  deriving (Eq, Show)

data ClassDecl = CL [Pred] Name [TVar] [Decl]
  deriving (Eq, Show)

data InstDecl = INST [Pred] Name Type [Decl]
  deriving (Eq, Show)

data Module = Module Name [Decl] -- module T
  deriving (Eq, Show)

-- helpers

var :: Name -> Type
var = TVar . TV

mkEApp :: [Expr] -> Expr
mkEApp es = foldl1 EApp es
