{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Parser where

import           Frontend
import           Lexer

import           Control.Applicative        (empty)
import           Control.Monad              (void)
import           Data.Char
import           Data.Text                  as T
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- Literal Parser

pIntLit :: Parser Literal
pIntLit = do
  i <- integer
  return $ LitInt i

pDoubleLit :: Parser Literal
pDoubleLit = do
  d <- double
  return $ LitDouble d

pLiteral :: Parser Literal
pLiteral = try pDoubleLit <|> pIntLit

-- lineComment :: Parser ()
-- lineComment = L.skipLineComment "#"

-- scn :: Parser ()
-- scn = L.space space1 lineComment empty

-- sc :: Parser ()
-- sc = L.space (void $ takeWhile1P Nothing f) lineComment empty
--   where
--     f x = x == ' ' || x == '\t'

-- lexeme :: Parser a -> Parser a
-- lexeme = L.lexeme sc

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

parseExpr :: T.Text -> Either String Expr
parseExpr = undefined
