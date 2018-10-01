{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (spec) where

import           Frontend
import           Lexer
import           Name
import           Parser
import           Type

import qualified Data.Text.Lazy                  as L
import qualified Data.Text.Lazy.IO               as LIO
import           System.IO.Unsafe

import           Test.Hspec
import           Test.Hspec.Expectations.Contrib

spec :: Spec
spec = do
  describe "Comments" $ do
    it "ignores comment" $ do
      parseSimpleUnlines pDecl ["f = # c1", "# c2", "  3 # c3"] `shouldBe` (Right $ FunDecl $ BindGroup
                                                             (Name "f")
                                                             [(Match [] [(ELit $ LitInt 3)])]
                                                             Nothing)

  describe "Types" $ do
    describe "Primitives" $ do
      it "int" $
        parseSimple pType "Int" `shouldBe` (Right $ tyInt)

      it "double" $
        parseSimple pType "Double" `shouldBe` (Right $ tyDouble)

      it "char" $
        parseSimple pType "Char" `shouldBe` (Right $ tyChar)

      it "bool" $
        parseSimple pType "Bool" `shouldBe` (Right $ tyBool)

    describe "Constructors" $ do
      it "no vars" $
        parseSimple pType "Maybe" `shouldBe` (Right $ mkTCon "Maybe")

      it "type variable" $
        parseSimple pType "Either a b" `shouldBe` (Right $
                                                  mkTApp "Either"
                                                  [TVar $ TV "a",
                                                   TVar $ TV "b"])

      it "type primitive" $
        parseSimple pType "Maybe Int" `shouldBe` (Right $
                                                 mkTApp "Maybe" [tyInt])

    describe "Arrow" $ do
      it "type variables" $
        parseSimple pType "a -> b" `shouldBe` (Right $
                                              mkTArr [TVar $ TV "a", TVar $ TV "b"])

      it "type primitive" $
        parseSimple pType "Int -> Char -> Bool" `shouldBe` (Right $
                                                   mkTArr [tyInt, tyChar, tyBool])

      it "type constructors" $
        parseSimple pType "Maybe a -> Either Int a" `shouldBe` (Right $
                                                               mkTArr
                                                               [ mkTApp "Maybe" [TVar $ TV "a"]
                                                               , mkTApp "Either" [tyInt, TVar $ TV "a"]
                                                               ])
    describe "Parens" $ do
      it "parens" $
        parseSimple pType "(Maybe a)" `shouldBe` (Right $
                                                  mkTApp "Maybe" [TVar $ TV "a"])
      
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
                                                 (EApp
                                                   (EInApp
                                                     (EVar $ Name "*")
                                                     (EVar $ Name "a"))
                                                   (ELit $ LitInt 3)))

    describe "Lambda" $ do
      it "single line, single param" $
        parseSimple pExpr "\\x -> y" `shouldBe` (Right $ ELam
                                               [Name "x"] [EVar $ Name "y"])

      it "single line, multi param" $
        parseSimple pExpr "\\x y -> x" `shouldBe` (Right $ ELam
                                                  [Name "x", Name "y"] [EVar $ Name "x"])

      it "multi line, multi param" $
        parseSimpleUnlines pExpr ["\\x y ->", "  1", "  y"] `shouldBe` (Right $ ELam
                                                                       [Name "x", Name "y"]
                                                                       [ELit $ LitInt 1, EVar $ Name "y"])

    describe "If" $ do
      it "single line" $
        parseSimple pExpr "if true then a else b" `shouldBe` (Right $ EIf
                                                             (ELit $ LitBool True)
                                                             [EVar $ Name "a"]
                                                             [EVar $ Name "b"])

      it "multi line" $
        parseSimpleUnlines pExpr ["if true", "then a", "else b"] `shouldBe` (Right $ EIf
                                                             (ELit $ LitBool True)
                                                             [EVar $ Name "a"]
                                                             [EVar $ Name "b"])

      it "multi line indent" $
        parseSimpleUnlines pExpr ["if true then", "  a", "  x", "else", "  b", "  z"] `shouldBe` (Right $ EIf
                                                                                          (ELit $ LitBool True)
                                                                                          [EVar $ Name "a", EVar $ Name "x"]
                                                                                          [EVar $ Name "b", EVar $ Name "z"])


    describe "Operators" $ do
      it "unary negation" $
        parseSimple pExpr "-a" `shouldBe` (Right $ EPreApp
                                          (EVar $ Name "-")
                                          (EVar $ Name "a"))

      it "unary not" $
        parseSimple pExpr "!a" `shouldBe` (Right $ EPreApp
                                          (EVar $ Name "!")
                                          (EVar $ Name "a"))

      it "addition" $
        parseSimple pExpr "1 + 2" `shouldBe` (Right $ EApp
                                             (EInApp
                                              (EVar $ Name "+")
                                              (ELit $ LitInt 1))
                                             (ELit $ LitInt 2))

      it "subtraction" $
        parseSimple pExpr "1 - 2" `shouldBe` (Right $ EApp
                                             (EInApp
                                               (EVar $ Name "-")
                                               (ELit $ LitInt 1))
                                             (ELit $ LitInt 2))

      it "multiplication" $
        parseSimple pExpr "1 * 2" `shouldBe` (Right $ EApp
                                             (EInApp
                                               (EVar $ Name "*")
                                               (ELit $ LitInt 1))
                                             (ELit $ LitInt 2))

      it "division" $
        parseSimple pExpr "1 / 2" `shouldBe` (Right $ EApp
                                             (EInApp
                                               (EVar $ Name "/")
                                               (ELit $ LitInt 1))
                                             (ELit $ LitInt 2))

      it "equal" $
        parseSimple pExpr "1 == 2" `shouldBe` (Right $ EApp
                                              (EInApp
                                                (EVar $ Name "==")
                                                (ELit $ LitInt 1))
                                              (ELit $ LitInt 2))

      it "less than" $
        parseSimple pExpr "1 < 2" `shouldBe` (Right $ EApp
                                             (EInApp
                                               (EVar $ Name "<")
                                               (ELit $ LitInt 1))
                                             (ELit $ LitInt 2))

      it "greater than" $
        parseSimple pExpr "1 > 2" `shouldBe` (Right $ EApp
                                             (EInApp
                                               (EVar $ Name ">")
                                               (ELit $ LitInt 1))
                                             (ELit $ LitInt 2))

      it "less than equal" $
        parseSimple pExpr "1 <= 2" `shouldBe` (Right $ EApp
                                              (EInApp
                                                (EVar $ Name "<=")
                                                (ELit $ LitInt 1))
                                              (ELit $ LitInt 2))

      it "greater than equal" $
        parseSimple pExpr "1 >= 2" `shouldBe` (Right $ EApp
                                              (EInApp
                                                (EVar $ Name ">=")
                                                (ELit $ LitInt 1))
                                              (ELit $ LitInt 2))

      it "and" $
        parseSimple pExpr "false && true" `shouldBe` (Right $ EApp
                                                     (EInApp
                                                       (EVar $ Name "&&")
                                                       (ELit $ LitBool False))
                                                     (ELit $ LitBool True))

      it "or" $
        parseSimple pExpr "false || true" `shouldBe` (Right $ EApp
                                                     (EInApp
                                                       (EVar $ Name "||")
                                                       (ELit $ LitBool False))
                                                     (ELit $ LitBool True))
      it "arithmetic presedence" $
        parseSimple pExpr "4 * -3 - 2 / 5" `shouldBe` (Right $ EApp
                                                      (EInApp
                                                        (EVar (Name "-"))
                                                        (EApp
                                                          (EInApp
                                                            (EVar (Name "*"))
                                                            (ELit (LitInt 4)))
                                                          (EPreApp
                                                            (EVar (Name "-"))
                                                            (ELit (LitInt 3)))))
                                                        (EApp
                                                          (EInApp
                                                            (EVar (Name "/"))
                                                            (ELit (LitInt 2)))
                                                          (ELit (LitInt 5))))

      it "binary presedence" $
        parseSimple pExpr "false && !true || false" `shouldBe` (Right $ EApp
                                                               (EInApp
                                                                 (EVar (Name "||"))
                                                                 (EApp
                                                                   (EInApp
                                                                     (EVar (Name "&&"))
                                                                     (ELit (LitBool False)))
                                                                   (EPreApp
                                                                     (EVar (Name "!"))
                                                                     (ELit (LitBool True)))))
                                                                 (ELit (LitBool False)))

    describe "Application" $ do
      it "single application" $
        parseSimple pExpr "x y" `shouldBe` (Right $ EApp (EVar $ Name "x") (EVar $ Name "y"))

      it "multiple application" $
        parseSimple pExpr "x y z" `shouldBe` (Right $ EApp
                                             (EApp (EVar $ Name "x") (EVar $ Name "y"))
                                             (EVar $ Name "z"))

    describe "Parens" $ do
      it "parens literal" $
        parseSimple pExpr "(3)" `shouldBe` (Right $ EParens $ ELit $ LitInt 3)

      it "parens arithmetic" $
        parseSimple pExpr "(a + 3)" `shouldBe` (Right $ EParens $ EApp
                                               (EInApp (EVar $ Name "+") (EVar $ Name "a"))
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

  describe "Function Decls" $ do
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

  describe "Module" $ do
    it "empty module" $
      parseSimple pModule "module Test" `shouldBe` (Right $ Module (Name "Test") [])

    it "module with declartions" $
      parseSimpleUnlines pModule ["module Test", "f _ = x"] `shouldBe` (Right $ Module
                                                                       (Name "Test")
                                                                       [FunDecl $ BindGroup (Name "f")
                                                                         [Match [PWild] [EVar $ Name "x"]]
                                                                       Nothing ])

    it "invalid module name" $
      isLeft $ parseSimple pModule "module test"

    it "reject indented declarations" $
      isLeft $ parseSimpleUnlines pModule ["module Test", "  f = x"]


testFile :: String -> FilePath
testFile s = "test/examples/" ++ s ++ ".yx"

parseSimpleUnlines :: Parser a -> [String] -> Either String a
parseSimpleUnlines p =
  parseSimple p . L.pack . unlines

parseSimpleFile :: Parser a -> FilePath -> Either String a
parseSimpleFile p f =
  parseSimple p text
  where text = unsafePerformIO $ LIO.readFile f
