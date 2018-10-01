module Types.Pred where

import           Name
import           Types.Type

-- qualified type
data Qual t = [Pred] :=> t
  deriving (Eq, Ord, Show)

-- predicate
data Pred = IsIn Name Type
  deriving (Eq, Ord, Show)
