{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Parser where

import           Frontend
import           Lexer
import           Name                       hiding (prefix)
import           Type

import           Data.Text.Lazy             as L
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Expr

-- Literal Parsers

pIntLit :: Parser Literal
pIntLit = LitInt <$> integer

pDoubleLit :: Parser Literal
pDoubleLit = LitDouble <$> double

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

-- Expressions

pName :: Parser Name
pName = Name <$> lowerIdentifier

pExprLiteral :: Parser Expr
pExprLiteral = ELit <$> pLiteral

pExprVar :: Parser Expr
pExprVar = EVar <$> pName

withExprBlock :: Parser ([Expr] -> Parser a) -> Parser a
withExprBlock p = try singleLine <|> multiLine
  where
    singleLine = do
      f <- p
      expr <- pExpr
      f [expr]
    multiLine = L.indentBlock scn p'
    p' = do
      f <- p
      return $ L.IndentSome Nothing f pExpr

pExprLam :: Parser Expr
pExprLam = withExprBlock p
  where
    p = do
      _ <- symbol "\\"
      names <- pName `sepBy` sc
      _ <- symbol "->"
      return $ return . ELam names

pExprIf :: Parser Expr
pExprIf = pIfBlock <*> pElseBlock

pIfBlock :: Parser ([Expr] -> Expr)
pIfBlock = withExprBlock p
  where
    p = do
      rword "if"
      cond <- pExpr
      scn
      -- TODO: Check indentation here to ensure >= rword "if" level
      rword "then"
      return $ return . EIf cond

pElseBlock :: Parser [Expr]
pElseBlock = withExprBlock p
  where
    p = do
      scn
      rword "else"
      return return

pExprAss :: Parser Expr
pExprAss = do
  name <- pName
  _ <- symbol "="
  expr <- pExpr
  return $ EAss name expr

pExprApp :: Parser Expr
pExprApp = do
  e1 <- pExpr
  e2 <- pExpr
  return $ EApp e1 e2

pExprParens :: Parser Expr
pExprParens = EParens <$> parens pExpr

mkInfix :: L.Text -> Expr -> Expr -> Expr
mkInfix name =
  EApp . EInApp (EVar $ Name $ L.unpack name)

mkPrefix :: L.Text -> Expr -> Expr
mkPrefix name =
  EPreApp (EVar $ Name $ L.unpack name)

binary :: String -> Operator Parser Expr
binary name = InfixL (mkInfix <$> symbol name)

prefix :: String -> Operator Parser Expr
prefix name = Prefix (mkPrefix <$> symbol name)

operators :: [[Operator Parser Expr]]
operators =
  [ [ prefix "-"
    , prefix "!" ]
  , [ binary "*"
    , binary "/" ]
  , [ binary "+"
    , binary "-" ]
  , [ binary "=="
    , binary "<="
    , binary ">="
    , binary "<"
    , binary ">" ]
  , [ binary "&&" ]
  , [ binary "||" ]]

aexpr :: Parser Expr
aexpr = do
  r <- some $ choice [ pExprParens
                     , pExprLiteral
                     , pExprVar
                     ]
  return $ Prelude.foldl1 EApp r

pExpr :: Parser Expr
pExpr = try pExprAss
  <|> pExprLam
  <|> pExprIf
  <|> makeExprParser aexpr operators

-- Patterns

pPatternLit :: Parser Pattern
pPatternLit = PLit <$> pLiteral

pPatternName :: Parser Pattern
pPatternName = PVar <$> pName

pPatternConstr :: Parser Pattern
pPatternConstr = do
  name <- upperIdentifier
  vars <- pPattern `sepBy` sc
  return $ PCon (Name name) vars

pPatternWild :: Parser Pattern
pPatternWild = symbol "_" >> return PWild

pPattern :: Parser Pattern
pPattern = pPatternLit
  <|> pPatternWild
  <|> try pPatternName
  <|> pPatternConstr
  <|> (lexeme . parens) pPattern

-- Functions

mkBind :: Name -> Maybe Type -> Match -> BindGroup
mkBind n t m = BindGroup n [m] t

pBindGroup :: Parser BindGroup
pBindGroup = withExprBlock p
  where
    p = do
      name <- pName
      pats <- pPattern `sepBy` sc
      _ <- symbol "="
      return (return
              . mkBind name Nothing
              . Match pats)

pFunctionDecl :: Parser Decl
pFunctionDecl = FunDecl <$> pBindGroup

-- Declarations

pDecl :: Parser Decl
pDecl = L.nonIndented scn pFunctionDecl

-- Module Parser

pModule :: Parser Module
pModule = L.nonIndented scn p
  where
    p = do
      rword "module"
      name <- Name <$> upperIdentifier
      decls <- many pDecl
      return $ Module name decls

contents :: Parser a -> Parser a
contents p = do
  r <- lexeme p
  eof
  return r

parseUnpack :: Either (ParseError Char Void) a -> Either String a
parseUnpack res = case res of
  Left err  -> Left $ parseErrorPretty err
  Right ast -> Right ast

parseModule :: String -> L.Text -> Either String Module
parseModule input = runStyxParser input pModule

parseExpr :: L.Text -> Either String Expr
parseExpr = parseSimple pExpr

parseSimple :: Parser a -> L.Text -> Either String a
parseSimple p = parseUnpack . runParser (contents p) "<stdin>" . L.strip

parseSimpleString :: Parser a -> String -> Either String a
parseSimpleString p = parseSimple p . L.pack

runStyxParser :: String -> Parser a -> L.Text -> Either String a
runStyxParser input p =
  parseUnpack . runParser (contents p) input . L.strip

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
--   in Prelude.unwords <$> ps <* sc

-- pItemList :: Parser (String, [(String, [String])])
-- pItemList = L.nonIndented scn (L.indentBlock scn p)
--   where
--     p = do
--       header <- pItem
--       return (L.IndentSome Nothing (return . (header, )) pComplexItem)

-- parser :: Parser (String, [(String, [String])])
-- parser = pItemList <* eof
