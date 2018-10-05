module Types.Scheme where

import qualified Data.Set as Set

import Name
import           Types.Pred
import           Types.Type

data Scheme = Forall [Name] (Qual Type)
  deriving (Eq, Ord, Show)

toScheme :: Type -> Scheme
toScheme t = Forall [] ([] :=> t)

instance FreeVars Scheme where
  freeVars (Forall ns _) = Set.fromList ns


