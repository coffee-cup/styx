{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Frontend where

import Prelude hiding (foldr, foldr1, concatMap)

import           Name
import           Type

type Constr = Name

data Expr
  = EApp Expr Expr               -- a b
  | EVar Name                    -- x
  | ELam Name Expr               -- \x -> y
  | ELit Literal                 -- 2, "hello"
  deriving (Eq, Show)

data Literal
  = LitInt Integer               -- 1
  | LitDouble Double             -- 1.1
  | LitBool Bool                 -- true, false
  | LitChar Char                 -- 'a'
  | LitString String             -- "hello"
  deriving (Eq, Ord, Show)

data BindGroup = BindGroup
  { _matchName  :: Name
  , _matchPats  :: [Match]
  , _matchType  :: Maybe Type
  } deriving (Eq, Show)

data Match = Match
  { _matchPat  :: [Pattern]
  , _matchBody :: Expr
  } deriving (Eq, Show)

data Pattern
  = PVar Name                    -- x
  | PCon Constr [Pattern]        -- C x y
  | PLit Literal                 -- 3
  | PWild                        -- _
  deriving (Eq, Show)

data Decl
  = FunDecl BindGroup            -- f x = x + 1
  | TypeDecl Type                -- f :: Int -> Int
  deriving (Eq, Show)

data Module = Module Name [Decl] -- module T export { .. }
  deriving (Eq, Show)
