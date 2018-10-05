module Types.Pred where

import qualified Data.Set as Set

import           Name
import           Types.Type

-- qualified type
data Qual t = [Pred] :=> t
  deriving (Eq, Ord, Show)

-- predicate
data Pred = IsIn Name Type
  deriving (Eq, Ord, Show)

instance FreeVars a => FreeVars (Qual a) where
  freeVars (_ :=> t) = freeVars t
