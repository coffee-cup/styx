module Name where

import           Control.Monad
import           Data.Monoid
import           Data.String

data Name
  = Gen String Integer
  | Name String
  deriving (Eq, Ord, Show, Read)

instance IsString Name where
  fromString = Name

prefix :: String -> Name -> Name
prefix p (Gen nm i) = Gen (p <> nm) i
prefix p (Name nm)  = Name (p <> nm)

unName :: IsString a => Name -> a
unName (Name s)  = fromString s
unName (Gen s n) = fromString (s ++ show n)

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

genNames :: [Name]
genNames = Prelude.zipWith Gen letters [0..]

class Named a where
  getName :: a -> Name
  getNameString :: a -> String
  getNameString a =
    let (Name n) = getName a
    in n
