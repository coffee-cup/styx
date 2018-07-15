{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (spec) where

import           Frontend
import           Lexer
import           Name
import           Parser

import qualified Data.Text.Lazy                  as L
import qualified Data.Text.Lazy.IO               as LIO
import           System.IO.Unsafe

import           Test.Hspec
import           Test.Hspec.Expectations.Contrib

spec :: Spec
spec = do
  describe "Literals" $ do
    it "integer" $
      parseSimple pLiteral "1" `shouldBe` (Right $ LitInt 1)

    it "double" $
      parseSimple pLiteral "1.1" `shouldBe` (Right $ LitDouble 1.1)

    it "bool true" $
      parseSimple pLiteral "true" `shouldBe` (Right $ LitBool True)

    it "bool false" $
      parseSimple pLiteral "false" `shouldBe` (Right $ LitBool False)

    it "char" $
      parseSimple pLiteral "'c'" `shouldBe` (Right $ LitChar 'c')

    it "string plain" $
      parseSimple pLiteral "\"hello\"" `shouldBe` (Right $ LitString "hello")

    it "string with newline" $
      parseSimple pLiteral "\"newline\n\"" `shouldBe` (Right $ LitString "newline\n")

    it "string with tab" $
      parseSimple pLiteral "\"tab\t\"" `shouldBe` (Right $ LitString "tab\t")

    it "string with escaped quotes" $
      parseSimple pLiteral "\"escaped\\\"\"" `shouldBe` (Right $ LitString "escaped\"")

  describe "Expressions" $ do
    describe "Literals" $ do
      it "literal integer" $
        parseSimple pExpr "1" `shouldBe` (Right $ ELit $ LitInt 1)

      it "literal double" $
        parseSimple pExpr "1.1" `shouldBe` (Right $ ELit $ LitDouble 1.1)

      it "literal bool" $
        parseSimple pExpr "true" `shouldBe` (Right $ ELit $ LitBool True)

      it "literal char" $
        parseSimple pExpr "'a'" `shouldBe` (Right $ ELit $ LitChar 'a')

      it "literal string" $
        parseSimple pExpr "\"abc\"" `shouldBe` (Right $ ELit $ LitString "abc")

    describe "Variables" $ do
      it "lowercase simple var" $
        parseSimple pExpr "a" `shouldBe` (Right $ EVar $ Name "a")

      it "underscore var" $
        parseSimple pExpr "_" `shouldBe` (Right $ EVar $ Name "_")

      it "invalid identifier" $
        isLeft $ parseSimple pExpr "#(&)%)#(&%#*(^%))1234"

    describe "Assignment" $ do
      it "assignment to literal" $
        parseSimple pExpr "x = 1" `shouldBe` (Right $ EAss
                                             (Name "x") (ELit $ LitInt 1))

      it "assignment to arithmetic" $
        parseSimple pExpr "x = a * 3" `shouldBe` (Right $ EAss
                                                 (Name "x")
                                                 (EBinaryOp (ABinary Mul) (EVar $ Name "a") (ELit $ LitInt 3)))

    describe "Operators" $ do
      it "unary negation" $
        parseSimple pExpr "-a" `shouldBe` (Right $ EUnaryOp
                                          (AUnary Neg)
                                          (EVar $ Name "a"))

      it "unary not" $
        parseSimple pExpr "!a" `shouldBe` (Right $ EUnaryOp
                                          (BUnary Not)
                                          (EVar $ Name "a"))

      it "addition" $
        parseSimple pExpr "1 + 2" `shouldBe` (Right $ EBinaryOp
                                             (ABinary Add)
                                             (ELit $ LitInt 1)
                                             (ELit $ LitInt 2))

      it "subtraction" $
        parseSimple pExpr "1 - 2" `shouldBe` (Right $ EBinaryOp
                                             (ABinary Sub)
                                             (ELit $ LitInt 1)
                                             (ELit $ LitInt 2))
      it "multiplication" $
        parseSimple pExpr "1 * 2" `shouldBe` (Right $ EBinaryOp
                                             (ABinary Mul)
                                             (ELit $ LitInt 1)
                                             (ELit $ LitInt 2))

      it "division" $
        parseSimple pExpr "1 / 2" `shouldBe` (Right $ EBinaryOp
                                             (ABinary Div)
                                             (ELit $ LitInt 1)
                                             (ELit $ LitInt 2))

      it "equal" $
        parseSimple pExpr "1 == 2" `shouldBe` (Right $ EBinaryOp
                                              (RBinary Equal)
                                              (ELit $ LitInt 1)
                                              (ELit $ LitInt 2))

      it "less than" $
        parseSimple pExpr "1 < 2" `shouldBe` (Right $ EBinaryOp
                                             (RBinary LessThan)
                                             (ELit $ LitInt 1)
                                             (ELit $ LitInt 2))

      it "greater than" $
        parseSimple pExpr "1 > 2" `shouldBe` (Right $ EBinaryOp
                                              (RBinary GreaterThan)
                                              (ELit $ LitInt 1)
                                              (ELit $ LitInt 2))

      it "less than equal" $
        parseSimple pExpr "1 <= 2" `shouldBe` (Right $ EBinaryOp
                                              (RBinary LessThanEqual)
                                              (ELit $ LitInt 1)
                                              (ELit $ LitInt 2))

      it "greater than equal" $
        parseSimple pExpr "1 >= 2" `shouldBe` (Right $ EBinaryOp
                                              (RBinary GreaterThanEqual)
                                              (ELit $ LitInt 1)
                                              (ELit $ LitInt 2))

      it "and" $
        parseSimple pExpr "false && true" `shouldBe` (Right $ EBinaryOp
                                                     (BBinary And)
                                                     (ELit $ LitBool False)
                                                     (ELit $ LitBool True))

      it "or" $
        parseSimple pExpr "false || true" `shouldBe` (Right $ EBinaryOp
                                                     (BBinary Or)
                                                     (ELit $ LitBool False)
                                                     (ELit $ LitBool True))
      it "arithmetic presedence" $
        parseSimple pExpr "4 * -3 - 2 / 5" `shouldBe` (Right $ EBinaryOp
                                                       (ABinary Sub)
                                                       (EBinaryOp (ABinary Mul)
                                                         (ELit (LitInt 4))
                                                         (EUnaryOp (AUnary Neg) (ELit (LitInt 3))))
                                                       (EBinaryOp (ABinary Div) (ELit (LitInt 2)) (ELit (LitInt 5))))
      it "binary presedence" $
        parseSimple pExpr "false && !true || false" `shouldBe` (Right $ EBinaryOp
                                                               (BBinary Or)
                                                               (EBinaryOp (BBinary And)
                                                                 (ELit (LitBool False))
                                                                 (EUnaryOp (BUnary Not) (ELit (LitBool True))))
                                                                (ELit (LitBool False)))

    describe "Parens" $ do
      it "parens literal" $
        parseSimple pExpr "(3)" `shouldBe` (Right $ ELit $ LitInt 3)

      it "parens arithmetic" $
        parseSimple pExpr "(a + 3)" `shouldBe` (Right $ EBinaryOp
                                               (ABinary Add)
                                               (EVar $ Name "a")
                                               (ELit $ LitInt 3))

  describe "Patterns" $ do
    it "literal" $
      parseSimple pPattern "32" `shouldBe` (Right $ PLit $ LitInt 32)

    it "name" $
      parseSimple pPattern "x" `shouldBe` (Right $ PVar $ Name "x")

    it "wild" $
      parseSimple pPattern "_" `shouldBe` (Right $ PWild)

    it "constructor no vars" $
      parseSimple pPattern "Hello" `shouldBe` (Right $ PCon (Name "Hello") [])

    it "constructor with single var" $
      parseSimple pPattern "Hello one" `shouldBe` (Right $ PCon (Name "Hello")
                                                   [PVar $ Name "one"])

    it "constructor with single var" $
      parseSimple pPattern "Hello _ 2.3" `shouldBe` (Right $ PCon (Name "Hello")
                                                     [PWild, PLit $ LitDouble 2.3])

  describe "Function Decls Single Line" $ do
    it "no patterns" $
      parseSimple pDecl "fn = 3" `shouldBe` (Right $ FunDecl $ BindGroup
                                                 (Name "fn")
                                                 [Match [] [(ELit $ LitInt 3)]]
                                                 Nothing)
    it "one simple pattern" $
      parseSimple pDecl "fn 3 = 3" `shouldBe` (Right $ FunDecl $ BindGroup
                                                    (Name "fn")
                                                    [(Match [PLit $ LitInt 3] [(ELit $ LitInt 3)])]
                                                    Nothing)

    it "two simple patterns" $
      parseSimple pDecl "fn f 2 = 3" `shouldBe` (Right $ FunDecl $ BindGroup
                                                     (Name "fn")
                                                     [(Match [PVar $ Name "f", PLit $ LitInt 2] [(ELit $ LitInt 3)])]
                                                     Nothing)

    it "complex constructor pattern" $
      parseSimple pDecl "fn f 2 _ (Hello b) (World 1.1 _) = x" `shouldBe` (Right $ FunDecl $ BindGroup
                                                                    (Name "fn")
                                                                    [(Match [
                                                                         PVar $ Name "f",
                                                                         PLit $ LitInt 2,
                                                                         PWild,
                                                                         PCon (Name "Hello") [PVar $ Name "b"],
                                                                         PCon (Name "World") [PLit $ LitDouble 1.1, PWild]]
                                                                     [(EVar $ Name "x")])]
                                                                      Nothing)

    it "const function" $
      parseSimpleFile pDecl (testFile "fundecl1") `shouldBe` (Right $ FunDecl $ BindGroup
                                                                (Name "const")
                                                                [(Match [PVar $ Name "x", PVar $ Name "y"] [(ELit $ LitInt 3)])]
                                                                Nothing)

  describe "Function Decls Double Line" $ do
    it "no patterns" $
      parseSimpleUnlines pDecl ["f =", "  3"] `shouldBe` (Right $ FunDecl $ BindGroup
                                                             (Name "f")
                                                             [(Match [] [(ELit $ LitInt 3)])]
                                                             Nothing)

    it "complex patterns" $
      parseSimpleUnlines pDecl ["test (Con 1 _ x) _ = ",
                                 "  x",
                                 "  1"] `shouldBe` (Right $ FunDecl $ BindGroup
                                                    (Name "test")
                                                    [Match [
                                                        PCon (Name "Con") [PLit $ LitInt 1, PWild, PVar $ Name "x"],
                                                        PWild]
                                                    [EVar $ Name "x", ELit $ LitInt 1]]
                                                    Nothing)

-- testParseDecls :: IO ()
-- testParseDecls = do
--   describe "Declarations should not be indented" $ do
--     it "Function errors when idented" $
--       isLeft $ parseSimple pDecl "  f = 3"

testFile :: String -> FilePath
testFile s = "test/examples/" ++ s ++ ".yx"

parseSimpleUnlines :: Parser a -> [String] -> Either String a
parseSimpleUnlines p =
  parseSimple p . L.pack . unlines

parseSimpleFile :: Parser a -> FilePath -> Either String a
parseSimpleFile p f =
  parseSimple p text
  where text = unsafePerformIO $ LIO.readFile f
