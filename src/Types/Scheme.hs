module Types.Scheme where

import           Name
import           Types.Kind
import           Types.Pred
import           Types.Type

data Scheme = Forall (Qual Type)
  deriving (Eq, Ord, Show)

toScheme :: Type -> Scheme
toScheme t = Forall ([] :=> t)
