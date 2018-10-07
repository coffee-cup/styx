module Types.Pred where

import qualified Data.Set    as Set

import           Name
import           Types.Subst
import           Types.Type

-- qualified type
data Qual t = [Pred] :=> t
  deriving (Eq, Ord, Show)

-- predicate
data Pred = IsIn Name Type
  deriving (Eq, Ord, Show)

instance FreeVars a => FreeVars (Qual a) where
  freeVars (_ :=> t) = freeVars t

instance (Substitutable t, FreeVars t) => Substitutable (Qual t) where
  apply s (ps :=> t) = apply s ps :=> apply s t
  ftv (_ :=> t) = Set.map TV $ freeVars t

instance Substitutable Pred where
  apply s (IsIn n t) = IsIn n (apply s t)
  ftv (IsIn _ t) = ftv t
