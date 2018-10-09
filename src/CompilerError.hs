module CompilerError where

data CompilerError
  = FileNotFound FilePath
  | ReplCommandError String
  | ParseError String
  deriving (Eq, Show)
