{-# LANGUAGE RecordWildCards #-}

module Types.Subst where

import qualified Data.Map   as Map
import qualified Data.Set   as Set

import           Name
import           Types.Type

newtype Subst = Subst (Map.Map TVar Type)

nullSubst :: Subst
nullSubst = Subst Map.empty

-- | map single variable u to type
(+->) :: TVar -> Type -> Subst
u +-> t = Subst $ Map.fromList [(u, t)]

-- | Combine substitutions by applying all substitutions mentioned
-- in the first argument to the type varaibles contained in the second
instance Monoid Subst where
  mappend subst1 subst2 = Subst (s1 `Map.union` s2)
    where
      Subst s1 = subst1
      Subst s2 = apply subst1 subst2

  mempty = nullSubst

class Substitutable a where
  -- replace every occurence of a type variable in the
  -- domain of the substitution with the corresponding type
  apply :: Subst -> a -> a

  -- return set of free variables occuring in its argument
  ftv :: a -> Set.Set TVar

instance (Substitutable a, Substitutable b) => Substitutable (a, b) where
  apply s (x, y) = (apply s x, apply s y)
  ftv (x, y) = ftv x `Set.union` ftv y

instance Substitutable a => Substitutable [a] where
  apply s = map (apply s)
  ftv a = Set.unions (fmap ftv a)

instance Substitutable Subst where
  apply s (Subst target) = Subst (fmap (apply s) target)
  ftv (Subst target) = Map.keysSet target

instance Substitutable Type where
  apply s t = case t of
    TVar a -> let Subst s' = s
              in Map.findWithDefault (TVar a) a s'
    ty@TCon{} -> ty
    TApp f x -> TApp (apply s f) (apply s x)
    TArr a b -> TArr (apply s a) (apply s b)

  ftv t = Set.map TV $ freeVars t
