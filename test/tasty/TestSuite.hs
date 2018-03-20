{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import RIO

import Test.Tasty                   (defaultMainWithIngredients, testGroup)
import Test.Tasty.Runners           (consoleTestReporter, listingTests)

import qualified TeardownTest as Teardown

main :: IO ()
main = defaultMainWithIngredients
  [listingTests, consoleTestReporter]
  (testGroup "teardown library" [Teardown.tests])
