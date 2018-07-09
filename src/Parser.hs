{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Parser where

import           Frontend
import           Lexer

import           Control.Applicative        (empty)
import           Control.Monad              (void)
import           Data.Char
import           Data.Text.Lazy             as L
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- Literal Parser

pIntLit :: Parser Expr
pIntLit = do
  i <- integer
  return $ ELit $ LitInt i

pDoubleLit :: Parser Expr
pDoubleLit = do
  d <- double
  return $ ELit $ LitDouble d

pBoolLit :: Parser Expr
pBoolLit =
  (rword "true" >> return (ELit $ LitBool True))
  <|> (rword "false" >> return (ELit $ LitBool False))

pCharLit :: Parser Expr
pCharLit = do
  _ <- char '\''
  c <- anyChar
  _ <- char '\''
  return $ ELit $ LitChar c

pStringLit :: Parser Expr
pStringLit = do
  _ <- char '"'
  x <- many $ escapedChars <|> noneOf ("\"\\" :: String)
  _ <- char '"'
  return $ ELit $ LitString x

pLiteral :: Parser Expr
pLiteral = try pDoubleLit
  <|> pIntLit
  <|> pBoolLit
  <|> pCharLit
  <|> pStringLit

expr :: Parser Expr
expr = pLiteral

parseUnpack :: Either (ParseError Char Void) Expr -> Either String Expr
parseUnpack res = case res of
  Left err -> Left $ parseErrorPretty err
  Right ast -> Right ast

parseExpr :: L.Text -> Either String Expr
parseExpr = parseUnpack . runParser expr "<stdin>" . L.strip


-- pItem :: Parser String
-- pItem = lexeme (takeWhile1P Nothing f) <?> "list item"
--   where
--     f x = isAlphaNum x || x == '-'

-- pComplexItem :: Parser (String, [String])
-- pComplexItem = L.indentBlock scn p
--   where
--     p = do
--       header <- pItem
--       return (L.IndentMany Nothing (return . (header, )) pLineFold)

-- pLineFold :: Parser String
-- pLineFold = L.lineFold scn $ \sc' ->
--   let ps = takeWhile1P Nothing f `sepBy1` try sc'
--       f x = isAlphaNum x || x == '-'
--   in unwords <$> ps <* sc

-- pItemList :: Parser (String, [(String, [String])])
-- pItemList = L.nonIndented scn (L.indentBlock scn p)
--   where
--     p = do
--       header <- pItem
--       return (L.IndentSome Nothing (return . (header, )) pComplexItem)

-- parser :: Parser (String, [(String, [String])])
-- parser = pItemList <* eof
