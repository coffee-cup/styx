module Types.Scheme where

import qualified Data.Map    as Map
import qualified Data.Set    as Set

import           Name
import           Types.Pred
import           Types.Subst
import           Types.Type

data Scheme = Forall (Set.Set TVar) (Qual Type)
  deriving (Eq, Ord, Show)

toScheme :: Type -> Scheme
toScheme t = Forall Set.empty ([] :=> t)

instance FreeVars Scheme where
  freeVars (Forall ns _) = Set.fromList $ map getName (Set.toList ns)

instance Substitutable Scheme where
  apply (Subst subst) (Forall ns qual) =
    let ns' = Map.fromSet (const ()) ns
        subst' = Subst (subst `Map.difference` ns')
    in Forall ns (apply subst' qual)

  ftv (Forall as t) = ftv t `Set.difference` as
