module Flags where

import           Data.Semigroup      ((<>))
import           Options.Applicative

data Flags = Flags
  { dumpFrontend :: Bool
  } deriving (Eq)

flagMap :: Flags -> [(String, Bool)]
flagMap Flags
         { dumpFrontend = frontend
         } = [ ("ddump-frontend", frontend)
             ]

instance Show Flags where
  show flags =
    foldl (\a f -> a ++ showIfNeeded f ++ "\n") "" (flagMap flags)
    where
      showIfNeeded :: (String, Bool) -> String
      showIfNeeded (s, True) = s
      showIfNeeded _         = ""

emptyFlags :: Flags
emptyFlags = Flags
  { dumpFrontend = True
  }

parseFlags :: Parser Flags
parseFlags = Flags
  <$> switch (  long "ddump-frontend"
             <> help "Print frontend AST to console")

setFlag :: String -> Bool -> Flags -> Maybe Flags
setFlag s b flags =
  case s of
    "ddump-frontend" -> Just $ flags { dumpFrontend = b }
    _              -> Nothing

