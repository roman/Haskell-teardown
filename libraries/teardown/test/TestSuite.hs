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

  , testCase "failing teardown action does not stop execution" $ do
      teardownAction <- newTeardown "failing teardown" $
        panic "failing teardown"

      result <- teardown teardownAction
      replicateM_ 9 (teardown teardownAction)

      assertBool "result should report an error"
                 (didTeardownFail result)

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
      assertEqual "teardown action must not be called more than once"
                  1 callCount

  , testCase "concatenated teardown actions keep idempotent guarantees" $ do
      callCountRefs <- replicateM 10 $ newIORef (0 :: Int)
      teardownActions <- forM callCountRefs $ \callCountRef ->
        newTeardown "test cleanup"
                    (atomicModifyIORef callCountRef (\a -> (succ a, ())))

      teardownAction <- concatTeardown "bigger system" teardownActions
      replicateM_ 10 (teardown teardownAction)

      countRefs <- mapM readIORef callCountRefs
      assertEqual "teardown action must not be called more than once"
                  (replicate 10 1)
                  countRefs

  , testCase "concatenated teardown actions return correct count" $ do
      teardownActions <-
        replicateM 10 (newTeardown "test cleanup" (return ()))

      teardownAction <- concatTeardown "bigger system" teardownActions
      toredownResult <- teardown teardownAction
      replicateM_ 9 (teardown teardownAction)

      assertEqual "teardown action must not be called more than once"
                  10 (toredownCount toredownResult)

  , testCase "concatenated failed teardown actions return correct count" $ do
      failedTeardownActions <-
        replicateM 5 (newTeardown "test cleanup with failures" (panic "nope"))

      teardownActions <-
        replicateM 5 (newTeardown "test cleanup" (return ()))

      teardownAction <-
        concatTeardown "bigger system"
                       (failedTeardownActions <> teardownActions)

      toredownResult <- teardown teardownAction
      replicateM_ 9 (teardown teardownAction)

      assertEqual "teardown action count must be correct"
                  10 (toredownCount toredownResult)

      assertEqual "failed teardown action must be correct"
                  5 (failedToredownCount toredownResult)

  ]
