{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Lexer where

import           Control.Applicative        hiding (many, some)
import           Control.Monad              (void)
import           Data.Char
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
sc = L.space (void $ takeWhile1P Nothing f) lineComment blockComment
  where
    f x = x == ' ' || x == '\t'

-- Consume whitespace after every lexeme (not before)
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- Parse a fixed string
symbol :: String -> Parser L.Text
symbol = L.symbol sc . L.pack

-- Parse something between parenthesis
parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

-- Parse something between braces
braces :: Parser a -> Parser a
braces = between (char '{') (char '}')

-- Parse an integer
integer :: Parser Integer
integer = lexeme L.decimal

-- Parse a double
double :: Parser Double
double = lexeme L.float

-- Common symbols

comma, equals, implies, pipe, dcolon, quote, dquote, bslash, arrow
  :: Parser ()

comma = void $ lexeme $ char ','
equals = void $ lexeme $ char '='
implies = void $ lexeme $ symbol "=>"
pipe = void $ lexeme $ char '|'
dcolon = void $ lexeme $ symbol "::"
quote = void $ lexeme $ char '\''
dquote = void $ lexeme $ char '"'
bslash = void $ lexeme $ char '\\'
arrow = void $ lexeme $ symbol "->"

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
reservedWords =
  [ "if"
  , "then"
  , "else"
  , "case"
  , "module"
  , "class"
  , "instance"
  , "type"
  , "where"
  ]

-- Parse a reserved word
rword :: String -> Parser ()
rword w = (lexeme . try) (string (L.pack w) *> notFollowedBy alphaNumChar)

-- Parse an identifier
identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> (oneOf $ letterCharUnder) <*> many (oneOf $ '_' : letterCharUnder ++ ['0'..'9'])
    letterCharUnder = '_' : ['a'..'z'] ++ ['A'..'Z']
    check x = if x `elem` reservedWords
                 then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                 else return x

-- Parse an identifier that passes a predicate
predIdentifier :: (String -> Bool) -> String -> Parser String
predIdentifier p err = identifier >>= check
  where
    check x = if p x
      then return x
      else fail $ "identifier " ++ x ++ " " ++ err

-- Parse an uppercase identifier
upperIdentifier :: Parser String
upperIdentifier = predIdentifier (p . head) "does not start with a uppercase letter"
  where
    p = flip elem ('_' : ['A'..'Z'])

-- Parse a lowercase identifier
lowerIdentifier :: Parser String
lowerIdentifier = predIdentifier (p . head) "does not start with a lowercase letter"
  where
    p = flip elem ('_' : ['a'..'z'])

-- Convert char in Parser to string
pCharToString :: Parser Char -> Parser L.Text
pCharToString pc = do
  c <- pc
  return $ L.pack $ show c
