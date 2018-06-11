module Flags where

import           Data.Semigroup      ((<>))
import           Options.Applicative

data Flags = Flags
  { dumpTokens :: Bool
  , dumpFrontend :: Bool
  } deriving (Eq)

flagMap :: Flags -> [(String, Bool)]
flagMap Flags
         { dumpTokens = tokens
         } = [("ddump-tokens", tokens)]

instance Show Flags where
  show flags =
    foldl (\a f -> a ++ showIfNeeded f ++ "\n") "" (flagMap flags)
    where
      showIfNeeded :: (String, Bool) -> String
      showIfNeeded (s, True) = s
      showIfNeeded _         = ""

emptyFlags :: Flags
emptyFlags = Flags
  { dumpTokens = True
  , dumpFrontend = True
  }

parseFlags :: Parser Flags
parseFlags = Flags
  <$> switch (  long "ddump-tokens"
             <> help "Print tokens to console")
  <*> switch (  long "ddump-frontend"
             <> help "Print frontend AST to console")

setFlag :: String -> Bool -> Flags -> Maybe Flags
setFlag s b flags =
  case s of
    "ddump-tokens" -> Just $ flags { dumpTokens = b }
    "ddump-frontend" -> Just $ flags { dumpFrontend = b }
    _              -> Nothing

