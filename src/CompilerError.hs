module CompilerError where

import Types.Infer

data CompilerError
  = FileNotFound FilePath
  | ReplCommandError String
  | ParseError String
  | InferError InferError
  deriving (Eq, Show)
