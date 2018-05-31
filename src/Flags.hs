module Flags where

import           Data.Semigroup         ((<>))
import           Options.Applicative

data Flags = Flags
  { dumpTokens :: Bool
  } deriving (Eq, Show)

emptyFlags :: Flags
emptyFlags = Flags
  { dumpTokens = False
  }

parseFlags :: Parser Flags
parseFlags = Flags
  <$> switch (  long "ddump-tokens"
             <> help "Print tokens to console")
