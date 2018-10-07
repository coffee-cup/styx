{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (spec) where

import           Frontend
import           Lexer
import           Name
import           Parser
import           Types.Pred
import           Types.Scheme
import           Types.Type

import qualified Data.Set                        as Set
import qualified Data.Text.Lazy                  as L
import qualified Data.Text.Lazy.IO               as LIO
import           System.IO.Unsafe

import           Test.Hspec
import           Test.Hspec.Expectations.Contrib

toTVarSet :: [String] -> Set.Set TVar
toTVarSet xs = Set.fromList (map (TV . Name) xs)

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

      it "unit"  $
        parseSimple pType "()" `shouldBe` (Right $ tyUnit)

    describe "Constructors" $ do
      it "no vars" $
        parseSimple pType "Maybe" `shouldBe` (Right $ mkTCon "Maybe")

      it "type variable" $
        parseSimple pType "Either a b" `shouldBe` (Right $
                                                  mkTApp "Either"
                                                  [var "a",
                                                   var "b"])

      it "type primitive" $
        parseSimple pType "Maybe Int" `shouldBe` (Right $
                                                 mkTApp "Maybe" [tyInt])

    describe "Arrow" $ do
      it "type variables" $
        parseSimple pType "a -> b" `shouldBe` (Right $
                                              mkTArr [var "a", var "b"])

      it "type primitive" $
        parseSimple pType "Int -> Char -> Bool" `shouldBe` (Right $
                                                   mkTArr [tyInt, tyChar, tyBool])

      it "type constructors" $
        parseSimple pType "Maybe a -> Either Int a" `shouldBe` (Right $
                                                               mkTArr
                                                               [ mkTApp "Maybe" [var "a"]
                                                               , mkTApp "Either" [tyInt, var "a"]
                                                               ])
    describe "Parens" $ do
      it "parens" $
        parseSimple pType "(Maybe a)" `shouldBe` (Right $
                                                  mkTApp "Maybe" [var "a"])

  describe "Literals" $ do
    it "integer" $
      parseSimple pLiteral "1" `shouldBe` (Right $ LitInt 1)

    it "double" $
      parseSimple pLiteral "1.1" `shouldBe` (Right $ LitDouble 1.1)

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

      it "literal char" $
        parseSimple pExpr "'a'" `shouldBe` (Right $ ELit $ LitChar 'a')

      it "literal string" $
        parseSimple pExpr "\"abc\"" `shouldBe` (Right $ ELit $ LitString "abc")

    describe "Variables" $ do
      it "lowercase simple var" $
        parseSimple pExpr "a" `shouldBe` (Right $ EVar $ Name "a")

      it "underscore var" $
        parseSimple pExpr "_" `shouldBe` (Right $ EVar $ Name "_")

      it "uppercase var" $
        parseSimple pExpr "Just" `shouldBe` (Right $ EVar $ Name "Just")

      it "invalid identifier" $
        isLeft $ parseSimple pExpr "#(&)%)#(&%#*(^%))1234"

    describe "Application" $ do
      it "simple application" $
        parseSimple pExpr "map f xs" `shouldBe` (Right $
                                                 mkEApp [EVar "map", EVar "f", EVar "xs"])

      it "constructor application" $
        parseSimple pExpr "Either Int a" `shouldBe` (Right $
                                                     mkEApp [EVar "Either", EVar "Int", EVar "a"])

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
        parseSimple pExpr "if True then a else b" `shouldBe` (Right $ EIf
                                                             (EVar "True")
                                                             [EVar $ Name "a"]
                                                             [EVar $ Name "b"])

      it "multi line" $
        parseSimpleUnlines pExpr ["if True", "then a", "else b"] `shouldBe` (Right $ EIf
                                                             (EVar "True")
                                                             [EVar $ Name "a"]
                                                             [EVar $ Name "b"])

      it "multi line indent" $
        parseSimpleUnlines pExpr ["if True then", "  a", "  x", "else", "  b", "  z"] `shouldBe` (Right $ EIf
                                                                                          (EVar "True")
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
        parseSimple pExpr "False && True" `shouldBe` (Right $ EApp
                                                     (EInApp
                                                       (EVar $ Name "&&")
                                                       (EVar "False"))
                                                     (EVar "True"))

      it "or" $
        parseSimple pExpr "False || True" `shouldBe` (Right $ EApp
                                                     (EInApp
                                                       (EVar $ Name "||")
                                                       (EVar "False"))
                                                     (EVar "True"))
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
        parseSimple pExpr "False && !True || False" `shouldBe` (Right $ EApp
                                                               (EInApp
                                                                 (EVar (Name "||"))
                                                                 (EApp
                                                                   (EInApp
                                                                     (EVar (Name "&&"))
                                                                     (EVar "False"))
                                                                   (EPreApp
                                                                     (EVar (Name "!"))
                                                                     (EVar "True"))))
                                                                 (EVar "False"))

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

  describe "Type Decls" $ do
    it "type with no predicates" $
      parseSimple pDecl "apply :: (a -> b) -> a -> b" `shouldBe` (Right $ TypeDecl "apply"
                                                                   (Forall (toTVarSet ["a", "b"]) ([] :=> mkTArr [
                                                                               mkTArr [var "a", var "b"],
                                                                               mkTArr [var "a", var "b"]])))

    it "type with predicates" $
      parseSimple pDecl "apply :: (Show a, Num b) => (a -> b) -> a -> b" `shouldBe` (Right $ TypeDecl "apply"
                                                                   (Forall (toTVarSet ["a", "b"]) ([IsIn "Show" (var "a"),
                                                                             IsIn "Num" (var "b")]
                                                                             :=> mkTArr
                                                                             [ mkTArr [var "a", var "b"]
                                                                             , mkTArr [var "a", var "b"]])))

  describe "Class Decls" $ do
    it "class with type decl, w/o predicates" $
      parseSimpleUnlines pDecl ["class Num a where",
                                "  plus :: a -> a -> a"] `shouldBe` (Right $ ClassDecl $
                                                                    CL [] "Num" [TV "a"]
                                                                    [TypeDecl "plus"
                                                                     (Forall (toTVarSet ["a"]) ([] :=> mkTArr [var "a", var "a", var "a"]))])

    it "class with type decl, w/predicates" $
      parseSimpleUnlines pDecl ["class Show a => Num a where",
                                "  plus :: a -> a -> a"] `shouldBe` (Right $ ClassDecl $
                                                                    CL [IsIn "Show" (var "a")] "Num" [TV "a"]
                                                                    [TypeDecl "plus"
                                                                     (Forall (toTVarSet ["a"]) ([] :=> mkTArr [var "a", var "a", var "a"]))])

    it "class with type decl and func decl" $
      parseSimpleUnlines pDecl ["class Show a => Num a where",
                                "  id :: a -> a",
                                "  id x = x"] `shouldBe` (Right $ ClassDecl $
                                                                    CL [IsIn "Show" (var "a")] "Num" [TV "a"]
                                                                    [TypeDecl "id"
                                                                     (Forall (toTVarSet ["a"]) ([] :=> mkTArr [var "a", var "a"])),
                                                                     FunDecl $ BindGroup (Name "id")
                                                                      [(Match [PVar "x"] [EVar "x"])]
                                                                      Nothing])

    it "class with multiline func decl" $
      parseSimpleUnlines pDecl ["class Show a => Num a where",
                                "  id x =",
                                "    x",
                                "    y"] `shouldBe` (Right $ ClassDecl $
                                                                    CL [IsIn "Show" (var "a")] "Num" [TV "a"]
                                                                    [FunDecl $ BindGroup (Name "id")
                                                                      [(Match [PVar "x"] [EVar "x", EVar "y"])]
                                                                      Nothing])

    it "class with no decls fails" $
      isLeft $ parseSimpleUnlines pDecl ["class Show a => Num a where"]

  describe "Instance Decls" $ do
    it "instance with no predicates" $
      parseSimpleUnlines pDecl ["instance Num a where",
                                "  id x = x"] `shouldBe` (Right $ InstDecl $
                                                         INST [] "Num" (var "a")
                                                           [FunDecl $ BindGroup (Name "id")
                                                            [(Match [PVar "x"] [EVar "x"])]
                                                             Nothing])

    it "instance with predicates" $
      parseSimpleUnlines pDecl ["instance First a, Second b => Num a where",
                                "  id x = x"] `shouldBe` (Right $ InstDecl $
                                                         INST [IsIn "First" (var "a"), IsIn "Second" (var "b")]
                                                           "Num" (var "a")
                                                           [FunDecl $ BindGroup (Name "id")
                                                            [(Match [PVar "x"] [EVar "x"])]
                                                             Nothing])

  describe "Data Decls" $ do
    it "single alias type" $
      parseSimple pDecl "type NewInt = Int" `shouldBe` (Right $ DataDecl $
                                                       DTCL "NewInt" [] [ConDecl "Int" []])

    it "constructor with type params" $
      parseSimple pDecl "type Either a b = Left a | Right b" `shouldBe` (Right $ DataDecl $
                                                                        DTCL "Either" ["a", "b"]
                                                                          [ConDecl "Left" [var "a"],
                                                                          ConDecl "Right" [var "b"]])

    it "multiline constructor" $
      parseSimpleUnlines pDecl ["type Either a b", "  = Left a", "  | Right b"] `shouldBe` (Right $ DataDecl $
                                                                        DTCL "Either" ["a", "b"]
                                                                          [ConDecl "Left" [var "a"],
                                                                          ConDecl "Right" [var "b"]])

    -- it "simple record type" $
    --   parseSimple pDecl "type Person = Person { name :: String }" `shouldBe` (Right $ DataDecl $
    --                                                                   DTCL "Person" []
    --                                                                          [RecDecl "Person" [("name", tyString)]])

    -- it "record with multiple fields and type params" $
    --   parseSimple pDecl "type Thing a = Thing { yes :: a, no :: Int }" `shouldBe` (Right $ DataDecl $
    --                                                                    DTCL "Thing" ["a"]
    --                                                                    [RecDecl "Thing" [("yes", var "a"),
    --                                                                                      ("no", tyInt)]])

    -- it "multiple constructor types" $
    --   parseSimple pDecl "type Person a b = P1 a | P2 { hello :: b }" `shouldBe` (Right $ DataDecl $
    --                                                                             DTCL "Person" ["a", "b"]
    --                                                                               [ConDecl "P1" [var "a"],
    --                                                                                RecDecl "P2" [("hello", var "b")]])

    -- it "multiline with multiple constructor types" $
    --   parseSimpleUnlines pDecl ["type Person a =", "  Jake", "  | Aleesha { v :: a }"] `shouldBe` (Right $ DataDecl $
    --                                                                                             DTCL "Person" ["a"]
    --                                                                                               [ConDecl "Jake" [],
    --                                                                                                RecDecl "Aleesha" [("v", var "a")]])

  describe "Module" $ do
    it "empty module" $
      parseSimple pModule "module Test" `shouldBe` (Right $ Module (Name "Test") [])

    it "module with declartions" $
      parseSimpleUnlines pModule ["module Test", "f _ = x"] `shouldBe` (Right $ Module
                                                                       (Name "Test")
                                                                       [FunDecl $ BindGroup (Name "f")
                                                                         [Match [PWild] [EVar $ Name "x"]]
                                                                       Nothing])

    it "valid file" $
       isRight $ parseSimpleFile pModule (testFile "all")

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
