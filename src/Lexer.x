{

{-# LANGUAGE FlexibleContexts #-}

module Lexer (
  Token(..),
  scanTokens
) where

import Control.Monad.Except

}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$eol   = [\n]

tokens :-

  -- Whitespace insensitive
  $eol                          ;
  $white+                       ;

  -- Comments
  "#".*                         ;

  -- Syntax
  $digit+                       { \s -> TokenInt (read s) }
  $digit+\.$digit*              { \s -> TokenDouble (read s) }
  "->"                          { \s -> TokenArrow }
  \=                            { \s -> TokenEq }
  \\                            { \s -> TokenLambda }
  [\+]                          { \s -> TokenAdd }
  [\-]                          { \s -> TokenSub }
  [\*]                          { \s -> TokenMul }
  \(                            { \s -> TokenLParen }
  \)                            { \s -> TokenRParen }
  $alpha [$alpha $digit \_ \']* { \s -> TokenSym s }

{

data Token
  = TokenInt Int
  | TokenDouble Double
  | TokenArrow
  | TokenEq
  | TokenLambda
  | TokenAdd
  | TokenSub
  | TokenMul
  | TokenLParen
  | TokenRParen
  | TokenSym String
  deriving (Eq, Show)

scanTokens :: String -> Except String [Token]
scanTokens str = go ('\n', [], str) where
  go inp@(_, _bs, str) =
    case alexScan inp 0 of
    AlexEOF -> return []
    AlexError _ -> throwError "Invalid lexeme."
    AlexSkip inp' len -> go inp'
    AlexToken inp' len act -> do
      res <- go inp'
      let rest = act (take len str)
      return (rest : res)

}
