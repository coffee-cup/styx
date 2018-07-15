{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Frontend where

import           Prelude hiding (concatMap, foldr, foldr1)

import           Name
import           Type

-- Constructor
type Constr = Name

data Expr
  = EApp Expr Expr               -- a b
  | EInApp Expr Expr             -- a + b
  | EPreApp Expr Expr            -- !a
  | EVar Name                    -- x
  | ELam Name Expr               -- \x -> y
  | EAss Name Expr               -- x = a
  | ELit Literal                 -- 2, "hello"
  | EIf Expr Expr Expr           -- if x then tr else fl
  | EAnn Expr Type               -- (x : Int)
  deriving (Eq, Show)

data Literal
  = LitInt Integer               -- 1
  | LitDouble Double             -- 1.1
  | LitBool Bool                 -- true, false
  | LitChar Char                 -- 'a'
  | LitString String             -- "hello"
  deriving (Eq, Ord, Show)

data BindGroup = BindGroup
  { _matchName :: Name
  , _matchPats :: [Match]
  , _matchType :: Maybe Type
  } deriving (Eq, Show)

data Match = Match
  { _matchPat  :: [Pattern]
  , _matchBody :: [Expr]
  } deriving (Eq, Show)

data Pattern
  = PVar Name                    -- x
  | PCon Constr [Pattern]        -- C x y
  | PLit Literal                 -- 3
  | PWild                        -- _
  deriving (Eq, Show)

-- data DataDecl
--   = ...

data Decl
  = FunDecl BindGroup            -- f x = x + 1
  | TypeDecl Type                -- f :: Int -> Int
  deriving (Eq, Show)

data Module = Module Name [Decl] -- module T
  deriving (Eq, Show)
