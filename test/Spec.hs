{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Frontend
import           Parser

import qualified Data.Text  as T
import           Test.Hspec

testLiterals :: IO ()
testLiterals = do
  hspec $ describe "Literals" $ do
    it "Integer" $
      parseExpr "1" `shouldBe` (Right $ ELit $ LitInt 1)

    it "Double" $
      parseExpr "1.1" `shouldBe` (Right $ ELit $ LitDouble 1.1)

    it "Bool True" $
      parseExpr "true" `shouldBe` (Right $ ELit $ LitBool True)

    it "Bool False" $
      parseExpr "false" `shouldBe` (Right $ ELit $ LitBool False)

    it "Char" $
      parseExpr "'c'" `shouldBe` (Right $ ELit $ LitChar 'c')

    it "String plain" $
      parseExpr "\"hello\"" `shouldBe` (Right $ ELit $ LitString "hello")

    it "String with newline" $
      parseExpr "\"newline\n\"" `shouldBe` (Right $ ELit $ LitString "newline\n")

    it "String with tab" $
      parseExpr "\"tab\t\"" `shouldBe` (Right $ ELit $ LitString "tab\t")

    it "String with escaped quotes" $
      parseExpr "\"escaped\\\"\"" `shouldBe` (Right $ ELit $ LitString "escaped\"")

main :: IO ()
main = testLiterals
