{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Protolude

import Test.Tasty                   (TestTree, defaultMainWithIngredients, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.Ingredients.Rerun (rerunningTests)
import Test.Tasty.Runners           (consoleTestReporter, listingTests)

import Control.Teardown
import Data.IORef       (atomicModifyIORef, newIORef, readIORef)

main :: IO ()
main =
  defaultMainWithIngredients
    [ rerunningTests [listingTests, consoleTestReporter] ]
    tests

tests :: TestTree
tests =
  testGroup "teardown"
  [
    testCase "idempotent execution of teardown action" $ do
      callCountRef <- newIORef (0 :: Int)
      teardownAction <- newTeardown "test cleanup" $
        atomicModifyIORef callCountRef (\a -> (succ a, ()))

      replicateM_ 10 (teardown teardownAction)
      callCount <- readIORef callCountRef
      assertEqual "teardown action got called more than once"
                  1 callCount

  , testCase "thread safe idempotent execution of teardown action" $ do
      callCountRef <- newIORef (0 :: Int)
      teardownAction <- newTeardown "test cleanup" $
        atomicModifyIORef callCountRef (\a -> (succ a, ()))

      asyncList <-
        replicateM 10 (async
                        -- each async executes teardown 3 times
                        $ replicateM_ 3
                        $ void $ teardown teardownAction)

      mapM_ wait asyncList
      callCount <- readIORef callCountRef
      assertEqual "teardown action got called more than once"
                  1 callCount

  , testCase "concatenated teardown actions keep idempotent guarantees" $ do
      callCountRefs <- replicateM 10 $ newIORef (0 :: Int)
      teardownActions <- forM callCountRefs $ \callCountRef ->
        newTeardown "test cleanup"
                    (atomicModifyIORef callCountRef (\a -> (succ a, ())))

      teardownAction <- concatTeardown "bigger system" teardownActions
      replicateM_ 10 (teardown teardownAction)

      countRefs <- mapM readIORef callCountRefs
      assertEqual "One teardown action is not idempotent when it should be"
                  (replicate 10 1)
                  countRefs
  ]
