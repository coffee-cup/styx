{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Parser where

import           Frontend
import           Lexer
import           Name                       hiding (prefix)
import           Types.Pred
import           Types.Scheme
import           Types.Type

import           Data.Text.Lazy             as L
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Expr

-- Literal Parsers

pIntLit :: Parser Literal
pIntLit = LitInt <$> integer <?> "integer"

pDoubleLit :: Parser Literal
pDoubleLit = LitDouble <$> double <?> "double"

pBoolLit :: Parser Literal
pBoolLit = p <?> "boolean"
  where
    p = (rword "true" >> return (LitBool True))
      <|> (rword "false" >> return (LitBool False))

pCharLit :: Parser Literal
pCharLit = do
  quote
  c <- anyChar
  quote
  (return $ LitChar c) <?> "char"

pStringLit :: Parser Literal
pStringLit = do
  dquote
  x <- many $ escapedChars <|> noneOf ("\"\\" :: String)
  dquote
  (return $ LitString x) <?> "string"

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
pExprLiteral = ELit <$> pLiteral <?> "literal"

pExprVar :: Parser Expr
pExprVar = EVar <$> pName <?> "variable"

pExprCon :: Parser Expr
pExprCon = (EVar . Name) <$> upperIdentifier

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
pExprLam = withExprBlock (p <?> "lambda")
  where
    p = do
      bslash
      names <- pName `sepBy` sc
      arrow
      return $ return . ELam names

pExprIf :: Parser Expr
pExprIf = pIfBlock <*> pElseBlock <?> "if expression"

pIfBlock :: Parser ([Expr] -> Expr)
pIfBlock = withExprBlock (p <?> "if block")
  where
    p = do
      rword "if"
      cond <- pExpr
      scn
      -- TODO: Check indentation here to ensure >= rword "if" level
      rword "then"
      return $ return . EIf cond

pElseBlock :: Parser [Expr]
pElseBlock = withExprBlock (p <?> "else block")
  where
    p = do
      scn
      rword "else"
      return return

pExprAss :: Parser Expr
pExprAss = p <?> "assignment"
  where
    p = do
      name <- pName
      equals
      expr <- pExpr
      return $ EAss name expr

pExprApp :: Parser Expr
pExprApp = p <?> "application"
  where
    p = do
      e1 <- pExpr
      e2 <- pExpr
      return $ EApp e1 e2

pExprParens :: Parser Expr
pExprParens = EParens <$> parens pExpr <?> "parens"

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
                     , try pExprVar
                     , pExprCon
                     ]
  return $ Prelude.foldl1 EApp r

pExpr :: Parser Expr
pExpr = try pExprAss
  <|> pExprLam
  <|> pExprIf
  <|> makeExprParser aexpr operators

-- Types

mkPrimParser :: Type -> Parser Type
mkPrimParser t = try p
  where
    p = do
      let n = getNameString t
      _ <- symbol n <?> n
      return t

pTypePrim :: Parser Type
pTypePrim = Prelude.foldl1 (<|>) (fmap mkPrimParser tyPrims)

pVar :: Parser TVar
pVar = (TV . Name) . (: []) <$> pc <?> "type variable"
  where
    pc = (try . lexeme) (lowerChar <* notFollowedBy alphaNumChar)

pVars :: Parser [TVar]
pVars = pVar `sepBy` sc

pTypeVar :: Parser Type
pTypeVar = do
  u <- Name . (: []) <$> lexeme lowerChar
  return (TVar $ TV u) <?> "type variable"

pTypeCon :: Parser Type
pTypeCon = do
  name <- Name <$> upperIdentifier
  let con = AlgTyCon name
  vars <- many $ choice [pTypeVar, pTypePrim]
  return (mkTApp con vars) <?> "type constructor"

pTypeParens :: Parser Type
pTypeParens = (lexeme . parens) pType

pType :: Parser Type
pType = makeExprParser atype typeOperators
  where
    typeOperators :: [[Operator Parser Type]]
    typeOperators = [ [ InfixR (mkTArr2 <$ symbol "->") ] ]
    atype = choice
      [ pTypeParens
      , try pTypePrim
      , pTypeVar
      , pTypeCon
      ]

-- Num a => Type
pPred :: Parser Pred
pPred = do
  name <- Name <$> upperIdentifier
  var <- pTypeVar
  return (IsIn name var) <?> "type predicate"

-- (Num a, Show b)
pPreds :: Parser [Pred]
pPreds = try p' <|> np
  where
    p = pPred `sepBy` comma
    p' = do
      ps <- (lexeme . parens) p <|> p
      implies
      return ps
    np = return []

pQual :: Parser (Qual Type)
pQual = do
  ps <- pPreds
  t <- pType
  return $ ps :=> t

pScheme :: Parser Scheme
pScheme = (try pq <|> pt) <?> "type scheme"
  where
    pq = Forall <$> pQual
    pt = toScheme <$> pType

-- Patterns

pPatternLit :: Parser Pattern
pPatternLit = PLit <$> pLiteral

pPatternName :: Parser Pattern
pPatternName = PVar <$> pName

pPatternConstr :: Parser Pattern
pPatternConstr = do
  name <- upperIdentifier
  vars <- pPattern `sepBy` sc
  return (PCon (Name name) vars) <?> "type constructor"

pPatternWild :: Parser Pattern
pPatternWild = symbol "_" >> return PWild <?> "wildcard"

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
      equals
      return (return
              . mkBind name Nothing
              . Match pats)

-- Declarations

pFunctionDecl :: Parser Decl
pFunctionDecl = FunDecl <$> pBindGroup <?> "function declaration"

pClassDecl :: Parser Decl
pClassDecl = L.indentBlock scn p' <?> "class declaration"
  where
    p :: Parser ([Decl] -> Parser Decl)
    p = do
      rword "class"
      preds <- pPreds
      name <- Name <$> upperIdentifier
      vars <- pVars
      rword "where"
      return $ return . ClassDecl . CL preds name vars
    p' = do
      f <- p
      return $ L.IndentSome Nothing f (choice [try pTypeDecl, pFunctionDecl])

pInstDecl :: Parser Decl
pInstDecl = L.indentBlock scn p' <?> "instance declaration"
  where
    p :: Parser ([Decl] -> Parser Decl)
    p = do
      rword "instance"
      preds <- pPreds
      name <- Name <$> upperIdentifier
      t <- pType
      rword "where"
      return $ return . InstDecl . INST preds name t
    p' = do
      f <- p
      return $ L.IndentSome Nothing f pFunctionDecl

pTypeDecl :: Parser Decl
pTypeDecl = do
  name <- pName
  dcolon
  s <- pScheme
  return (TypeDecl name s) <?> "type declaration"

pConDecl :: Parser ConDecl
pConDecl = pCon
  where
    pCon = do
      name <- Name <$> upperIdentifier
      ts <- many pType
      try scn
      return $ ConDecl name ts
    pRec = do
      name <- Name <$> upperIdentifier
      _ <- symbol "{"
      nts <- pNamedType `sepBy` comma
      _ <- symbol "}"
      return $ RecDecl name nts
    pNamedType :: Parser (Name, Type)
    pNamedType = do
      name <- pName
      dcolon
      t <- pType
      return (name, t)

pDataDecl :: Parser Decl
pDataDecl = do
  rword "type"
  name <- Name <$> upperIdentifier
  vars <- many pVar
  try scn
  equals
  try scn
  cons <- pConDecl `sepBy` (pipe >> try scn)
  return $ DataDecl $ DTCL name vars cons

pDecl :: Parser Decl
pDecl = L.nonIndented scn p
  where
    p = choice
      [ try pFunctionDecl
      , try pClassDecl
      , try pInstDecl
      , try pDataDecl
      , pTypeDecl
      ]

-- Module Parser

pModule :: Parser Module
pModule = L.nonIndented scn (p <?> "module definition")
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
