module Types.Infer where

import Name
import Types.Type

data InferError
  -- | Two types that don't match were attempted to be unified
  = CannotUnify Type Type

  -- | A TVar is bound to a Type that already contains it
  | OccursCheckFailed Name Type

  -- | The value of an unknow identifier was read
  | UnknownIdentifier Name
  deriving (Show)
