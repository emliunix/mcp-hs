{-# LANGUAGE OverloadedStrings #-}
module JsonRpc.TestStdIO where

import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec
import qualified Data.ByteString as B
import JsonRpc.StdIO (bytes)

spec :: Spec
spec = do
  describe "JsonRpc.StdIO.bytes" $ do
    it "splits ByteString correctly" $ do
      bytes ["hello", "world"] 7 `shouldBe` ("worldhe", ["llo"])
  
  describe "JsonRpc.StdIO.feed" $ do
    it "emptyTest" $ do
      True `shouldBe` True
