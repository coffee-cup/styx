{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Lexer where

import           Control.Applicative        hiding (many)
import qualified Data.Text.Lazy             as L
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void L.Text

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

blockComment :: Parser ()
blockComment = empty

-- Space and newlines
scn :: Parser ()
scn = L.space space1 lineComment empty

-- Only spaces
sc :: Parser ()
sc = L.space space1 lineComment blockComment

-- Consume whitespace after every lexeme (not before)
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- Parse a fixed string
symbol :: String -> Parser L.Text
symbol = L.symbol sc . L.pack

-- Parse something between parenthesis
parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

-- Parse an integer
integer :: Parser Integer
integer = lexeme L.decimal

-- Parse a double
double :: Parser Double
double = lexeme L.float

-- Parse an escaped character
escapedChars :: Parser Char
escapedChars = do
  _ <- char '\\'                   -- a backslash
  x <- oneOf ("\\\"nrt" :: String) -- either backslash or doublequote
  return $ case x of
    '\\' -> x
    '"'  -> x
    'n'  -> '\n'
    'r'  -> '\r'
    't'  -> '\t'
    _    -> x

-- List of reserved words
reservedWords :: [String]
reservedWords = ["if", "then", "else", "case"]

-- Parse a reserved word
rword :: String -> Parser ()
rword w = (lexeme . try) (string (L.pack w) *> notFollowedBy alphaNumChar)

-- Parse an identifier
identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` reservedWords
                 then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                 else return x
