{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Pretty where

import qualified Frontend         as S
import           Name

import           Text.PrettyPrint

class Pretty p where
  ppr :: Int -> p -> Doc

  {-# INLINE pp #-}
  pp :: p -> Doc
  pp = ppr 0

  {-# INLINE ppg #-}
  ppg :: p -> String
  ppg = render . pp

instance Pretty Name where
  ppr _ (Name x)   = text x
  ppr _ (Gen nm i) = pp nm <> integer i

instance Pretty String where
  ppr _ x = text x

instance Pretty Int where
  ppr _ x = int x


-- Printer Utils

spaced :: Pretty a => Int -> [a] -> Doc
spaced p = hsep . fmap (ppr p)

parensIf :: Bool -> Doc -> Doc
parensIf True  = parens
parensIf False = id

tysig :: Pretty a => Name -> a -> Doc
tysig f ty = pp f <+> "::" <+> pp ty

spaces :: Int -> String
spaces n
  | n <= 0    = ""
  | otherwise = replicate n ' '

indent :: Int -> Doc -> Doc
indent i d = hang (text (spaces i) <> d) i empty

block :: Pretty a => [a] -> Doc
block xs =
  char '{'
  $$ nest 2 (vcat (punctuate semi (fmap pp xs)))
  $$ char '}'

commafy :: [Doc] -> Doc
commafy = hsep . punctuate comma

ppmaybe :: Pretty a => Maybe a -> Doc
ppmaybe = maybe empty pp

banner :: String
banner = render $
  text (ascii ++
  " Styx Compiler 0.1.0\n")
  where
    ascii = "  ___ _            \r\n / __| |_ _  ___ __\r\n \\__ \\  _| || \\ \\ /\r\n |___/\\__|\\_, /_\\_\\\r\n          |__/     \r\n\n"
