{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Tasty
import Test.Tasty.Hspec
import qualified JsonRpc.TestNoJSON (spec)
import qualified JsonRpc.TestStdIO (spec)

main :: IO ()
main = do
  testSpecsNoJSON <- testSpecs JsonRpc.TestNoJSON.spec
  testSpecsStdIO <- testSpecs JsonRpc.TestStdIO.spec
  defaultMain (testGroup "Tests" (testSpecsNoJSON ++ testSpecsStdIO))
