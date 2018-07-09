{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Frontend
import           Name
import           Parser

import qualified Data.Text  as T
import           Test.Hspec

testLiterals :: IO ()
testLiterals = do
  hspec $ describe "Literals" $ do
    it "Integer" $
      parseSimple pLiteral "1" `shouldBe` (Right $ LitInt 1)

    it "Double" $
      parseSimple pLiteral "1.1" `shouldBe` (Right $ LitDouble 1.1)

    it "Bool True" $
      parseSimple pLiteral "true" `shouldBe` (Right $ LitBool True)

    it "Bool False" $
      parseSimple pLiteral "false" `shouldBe` (Right $ LitBool False)

    it "Char" $
      parseSimple pLiteral "'c'" `shouldBe` (Right $ LitChar 'c')

    it "String plain" $
      parseSimple pLiteral "\"hello\"" `shouldBe` (Right $ LitString "hello")

    it "String with newline" $
      parseSimple pLiteral "\"newline\n\"" `shouldBe` (Right $ LitString "newline\n")

    it "String with tab" $
      parseSimple pLiteral "\"tab\t\"" `shouldBe` (Right $ LitString "tab\t")

    it "String with escaped quotes" $
      parseSimple pLiteral "\"escaped\\\"\"" `shouldBe` (Right $ LitString "escaped\"")

testPatterns :: IO ()
testPatterns = do
  hspec $ describe "Patterns" $ do
    it "Literal" $
      parseSimple pPattern "32" `shouldBe` (Right $ PLit $ LitInt 32)

    it "Name" $
      parseSimple pPattern "x" `shouldBe` (Right $ PVar $ Name "x")

    it "Wild" $
      parseSimple pPattern "_" `shouldBe` (Right $ PWild)

    it "Constructor no vars" $
      parseSimple pPattern "Hello" `shouldBe` (Right $ PCon (Name "Hello") [])

    it "Constructor with single var" $
      parseSimple pPattern "Hello one" `shouldBe` (Right $ PCon (Name "Hello")
                                                   [PVar $ Name "one"])

    it "Constructor with single var" $
      parseSimple pPattern "Hello _ 2.3" `shouldBe` (Right $ PCon (Name "Hello")
                                                     [PWild, PLit $ LitDouble 2.3])

main :: IO ()
main = do
  testLiterals
  testPatterns
