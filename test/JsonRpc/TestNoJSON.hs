{-# LANGUAGE OverloadedStrings #-}
module JsonRpc.TestNoJSON where

import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec
import Data.Attoparsec.ByteString (parseOnly)
import qualified Data.ByteString.Char8 as B
import JsonRpc.NoJSON (pNull, pBool, pNumber, pString, pArray, pObject)

spec :: Spec
spec = do
  describe "JsonRpc.NoJSON" $ do
    it "parses null" $ do
      parseOnly pNull "null" `shouldBe` Right 4

    it "parses boolean" $ do
      parseOnly pBool "true" `shouldBe` Right 4
      parseOnly pBool "false" `shouldBe` Right 5

    it "parses numbers" $ do
      parseOnly pNumber "123" `shouldBe` Right 3
      parseOnly pNumber "-123.45e+6" `shouldBe` Right 10
      parseOnly pNumber "0" `shouldBe` Right 1
      parseOnly pNumber "-0" `shouldBe` Right 2
      parseOnly pNumber "0.001" `shouldBe` Right 5

    it "parses string literal" $ do
      parseOnly pString "\"Smiling face: \\uD83D\\uDE00\"" `shouldBe` Right 28

    it "parses escaped characters in strings" $ do
      parseOnly pString "\"Hello, \\\"world\\\"!\"" `shouldBe` Right 19
      parseOnly pString "\"Backslash: \\\\ and newline: \\n\"" `shouldBe` Right 31
      parseOnly pString "\"Tab: \\t and Unicode: \\u03A9\"" `shouldBe` Right 29

    it "parses arrays" $ do
      parseOnly pArray "[ 1, 2, 3]" `shouldBe` Right 10
      parseOnly pArray "[\"a\", \"b\", \"c\"]" `shouldBe` Right 15
      parseOnly pArray "[true , false, null]" `shouldBe` Right 20
      parseOnly pArray "[]" `shouldBe` Right 2

    it "parses objects" $ do
      parseOnly pObject "{\"key\"  : \"value\"}" `shouldBe` Right 18
      parseOnly pObject "{\"name\": \"Alice\", \"age\": 30}" `shouldBe` Right 28
      parseOnly pObject "{\"nested\": {\"key\": \"value\"}}" `shouldBe` Right 28
      parseOnly pObject "{\"array\": [1, 2, 3]}" `shouldBe` Right 20
      parseOnly pObject "{}" `shouldBe` Right 2
