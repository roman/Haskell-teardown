{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import RIO

import Test.Tasty                   (defaultMainWithIngredients, testGroup)
import Test.Tasty.Ingredients.Rerun (rerunningTests)
import Test.Tasty.Runners           (consoleTestReporter, listingTests)

import qualified ComponentTest as Component
import qualified TeardownTest  as Teardown

main :: IO ()
main = defaultMainWithIngredients
  [rerunningTests [listingTests, consoleTestReporter]]
  (testGroup "teardown library" [Teardown.tests, Component.tests])
