{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Parser where

import           Frontend
import           Lexer
import           Name

import           Control.Applicative        (empty)
import           Control.Monad              (void)
import           Data.Char
import           Data.Text.Lazy             as L
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- Literal Parsers

pIntLit :: Parser Literal
pIntLit = do
  i <- integer
  return $ LitInt i

pDoubleLit :: Parser Literal
pDoubleLit = do
  d <- double
  return $ LitDouble d

pBoolLit :: Parser Literal
pBoolLit =
  (rword "true" >> return (LitBool True))
  <|> (rword "false" >> return (LitBool False))

pCharLit :: Parser Literal
pCharLit = do
  _ <- char '\''
  c <- anyChar
  _ <- char '\''
  return $ LitChar c

pStringLit :: Parser Literal
pStringLit = do
  _ <- char '"'
  x <- many $ escapedChars <|> noneOf ("\"\\" :: String)
  _ <- char '"'
  return $ LitString x

pLiteral :: Parser Literal
pLiteral = try pDoubleLit
  <|> pIntLit
  <|> pBoolLit
  <|> pCharLit
  <|> pStringLit

pLiteralExpr :: Parser Expr
pLiteralExpr = do
  lit <- pLiteral
  return $ ELit lit

-- Patterns

pPatternLit :: Parser Pattern
pPatternLit = do
  lit <- pLiteral
  return $ PLit lit

pPatternName :: Parser Pattern
pPatternName = do
  name <- lowerIdentifier
  return $ PVar $ Name name

pPatternConstr :: Parser Pattern
pPatternConstr = do
  name <- upperIdentifier
  vars <- pPattern `sepBy` sc
  return $ PCon (Name name) vars

pPatternWild :: Parser Pattern
pPatternWild = char '_' >> return PWild

pPattern :: Parser Pattern
pPattern = pPatternLit
  <|> try pPatternName
  <|> pPatternConstr
  <|> pPatternWild
  <|> (lexeme . parens) pPattern

-- Matches

pMatch :: Char -> Parser Match
pMatch sep = do
  pats <- pPattern `sepBy` sc
  _ <- lexeme $ char sep
  expr <- pExpr
  return $ Match pats expr

-- BindGroups

pBindGroup :: Parser BindGroup
pBindGroup = do
  name <- lowerIdentifier
  match <- pMatch '='
  return $ BindGroup (Name name) [match] Nothing

-- Functions

-- pFunctionDecl :: Parser Decl
-- pFunctionDecl =
--   name <- lowerIdentifier

-- Module Parser

-- pModule :: Parser Module
-- pModule = do
--   rword "module"
--   name <- upperIdentifier


pExpr :: Parser Expr
pExpr = pLiteralExpr

contents :: Parser a -> Parser a
contents p = do
  scn
  r <- try $ lexeme p
  eof
  return r

parseUnpack :: Either (ParseError Char Void) a -> Either String a
parseUnpack res = case res of
  Left err  -> Left $ parseErrorPretty err
  Right ast -> Right ast

parseExpr :: L.Text -> Either String Expr
parseExpr = parseUnpack . runParser (contents pExpr) "<stdin>" . L.strip

parseSimple :: Parser a -> L.Text -> Either String a
parseSimple p = parseUnpack . runParser (contents p) "<stdin>" . L.strip

parseSimpleString :: Parser a -> String -> Either String a
parseSimpleString p = parseUnpack . runParser (contents p) "<stdin>" . L.strip . L.pack

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
