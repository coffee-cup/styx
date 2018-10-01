module Types.Kind where

import Name

data Kind
  = KStar
  | KArr Kind Kind
  | KVar Name
  deriving (Eq, Ord, Show)
