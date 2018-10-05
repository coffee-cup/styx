{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Frontend where

import           Data.List    (foldl')
import           Data.Map     as Map hiding (foldl, partition)
import qualified Data.Set     as Set
import           Prelude      hiding (concatMap, foldr, foldr1)

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
  | LitChar Char                 -- 'a'
  | LitString String             -- "hello"
  deriving (Eq, Ord, Show)

-- BindGroups

data BindGroup = BindGroup
  { _matchName   :: Name
  , _matchPats   :: [Match]
  , _matchScheme :: Maybe Scheme
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

-- Declarations

data Decl
  = FunDecl BindGroup             -- f x = x + 1
  | TypeDecl Name Scheme          -- f :: Int -> Int
  | ClassDecl ClassDecl           -- class (P) => T where { ... }
  | InstDecl InstDecl             -- instance (P) => T where { ... }
  | DataDecl DataDecl             -- data Maybe where { ... }
  deriving (Eq, Show)

data ClassDecl = CL [Pred] Name [TVar] [Decl]
  deriving (Eq, Show)

data InstDecl = INST [Pred] Name Type [Decl]
  deriving (Eq, Show)

data DataDecl = DTCL Constr [TVar] [ConDecl]
  deriving (Eq, Show)

data ConDecl
  = ConDecl Constr [Type]         -- T a b
  | RecDecl Constr [(Name, Type)] -- { label :: a }
  deriving (Eq, Show)

data Module = Module Name [Decl]  -- module T
  deriving (Eq, Show)

-- Helpers

mkEApp :: [Expr] -> Expr
mkEApp es = foldl1 EApp es

mkEVar :: String -> Expr
mkEVar = EVar . Name

-- Variables

instance AllVars Pattern where
  allVars pt = case pt of
    PVar n    -> Set.singleton n
    PCon _ ps -> allVars ps
    PLit _    -> Set.empty
    PWild     -> Set.empty

instance AllVars Match where
  allVars (Match _ rhs) = allVars rhs

instance AllVars Expr where
  allVars ex = case ex of
    EApp a b    -> allVars a `Set.union` allVars b
    EInApp a b  -> allVars a `Set.union` allVars b
    EPreApp a b -> allVars a `Set.union` allVars b
    EVar a      -> Set.singleton a
    ELam _ xs   -> allVars xs
    EAss n e    -> Set.singleton n `Set.union` freeVars e
    ELit _      -> Set.empty
    EIf c x y   -> Set.unions [freeVars c, freeVars x, freeVars y]
    EAnn e _    -> freeVars e
    EParens e   -> freeVars e

instance AllVars Decl where
  allVars (FunDecl bg) = allVars bg
  allVars DataDecl {}  = Set.empty
  allVars TypeDecl {}  = Set.empty
  allVars ClassDecl {} = Set.empty
  allVars InstDecl {}  = Set.empty

instance AllVars BindGroup where
  allVars (BindGroup _ pats _) = Set.unions (fmap allVars pats)

instance FreeVars Expr where
  freeVars ex = case ex of
    EApp a b    -> freeVars a `Set.union` freeVars b
    EInApp a b  -> freeVars a `Set.union` freeVars b
    EPreApp a b -> freeVars a `Set.union` freeVars b
    EVar a      -> Set.singleton a
    ELam ns xs  -> freeVars xs Set.\\ Set.fromList ns
    EAss _ e    -> freeVars e
    ELit _      -> Set.empty
    EIf c x y   -> Set.unions [freeVars c, freeVars x, freeVars y]
    EAnn e _    -> freeVars e
    EParens e   -> freeVars e

instance FreeVars Match where
  freeVars ex = case ex of
    Match pats rhs -> freeVars rhs Set.\\ Set.unions (fmap allVars pats)

instance FreeVars Decl where
  freeVars (FunDecl bg) = freeVars bg
  freeVars DataDecl {}  = Set.empty
  freeVars TypeDecl {}  = Set.empty
  freeVars ClassDecl {} = Set.empty
  freeVars InstDecl {}  = Set.empty

instance FreeVars BindGroup where
  freeVars (BindGroup _ pats _) = Set.unions (fmap freeVars pats)

occursIn :: AllVars a => Name -> a -> Bool
occursIn name ex = name `Set.member` (allVars ex)

boundVars :: (FreeVars a, AllVars a) => a -> Set.Set Name
boundVars ex = (allVars ex) `Set.difference` (freeVars ex)

-- Toplevel

-- | Groups related type and function declarations into single BindGroup
groupTopLevel :: Module -> Module
groupTopLevel (Module n decls) = Module n $ groupTopLevelDecls decls

groupTopLevelDecls :: [Decl] -> [Decl]
groupTopLevelDecls decls =
    let tfDecls    = [d | d <- decls, isTypeOrFunDecl d]
        dataDecls  = [d | d@DataDecl{} <- decls]
        classDecls = [d | d@ClassDecl{} <- decls]
        instDecls  = [d | d@InstDecl{} <- decls]
        groupedBindings = foldl createGroupBGs Map.empty tfDecls
        groupedFunDecls = fmap (FunDecl . snd) (Map.toList groupedBindings)
    in mconcat [classDecls, instDecls, dataDecls, groupedFunDecls]


isTypeOrFunDecl :: Decl -> Bool
isTypeOrFunDecl (FunDecl _)    = True
isTypeOrFunDecl (TypeDecl _ _) = True
isTypeOrFunDecl _              = False

createGroupBGs :: Map.Map Name BindGroup -> Decl -> Map Name BindGroup
createGroupBGs m (FunDecl bg@(BindGroup name matches _)) =
  case Map.lookup name m of
    Just _ ->
      Map.adjust (\bg' -> bg' { _matchPats =
                                _matchPats bg' ++ matches }) name m
    Nothing ->
      Map.insert name bg m
createGroupBGs m (TypeDecl name sc) =
  case Map.lookup name m of
    Just _ ->
      Map.adjust (\bg -> bg { _matchScheme = Just sc}) name m

    Nothing ->
      Map.insert name (BindGroup name [] (Just sc)) m
createGroupBGs _ _ =
  error "cannot createGroupBGs for this type of declaration"


-- Deconstructors

viewVars :: Expr -> [Name]
viewVars (ELam ns _) = ns
viewVars _           = []

-- viewLam :: Expr -> Expr
-- viewLam (ELam _ a) = viewLam a
-- viewLam x          = x

viewApp :: Expr -> (Expr, [Expr])
viewApp = go []
  where
    go !xs (EApp a b)    = go (b : xs) a
    go !xs (EInApp a b)  = go (a : xs) b
    go !xs (EPreApp a b) = go (b : xs) a
    go xs f              = (f, xs)
