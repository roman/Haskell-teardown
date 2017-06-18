{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Protolude

import Test.Tasty                   (TestTree, defaultMainWithIngredients, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.Ingredients.Rerun (rerunningTests)
import Test.Tasty.Runners           (consoleTestReporter, listingTests)

import Control.Teardown
import Data.IORef       (atomicModifyIORef, newIORef, readIORef, modifyIORef)

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
        (panic "failing teardown" :: IO ())

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

  , testCase "teardown tree keeps idempotent guarantees around execution" $ do
      callCountRefs <- replicateM 10 $ newIORef (0 :: Int)

      teardownAction <-
        newTeardown "bigger system" $ do
          forM callCountRefs $ \callCountRef ->
            newTeardown "test cleanup"
                        (atomicModifyIORef callCountRef (\a -> (succ a, ())))

      replicateM_ 10 (teardown teardownAction)

      countRefs <- mapM readIORef callCountRefs
      assertEqual "teardown action must not be called more than once"
                  (replicate 10 1)
                  countRefs

  , testCase "teardown action that returns Teardown list returns correct count" $ do
      failedTeardownActions <-
        replicateM 5 (newTeardown "test cleanup with failures" (panic "nope" :: IO ()))

      teardownActions <-
        replicateM 5 (newTeardown "test cleanup" (return () :: IO ()))

      teardownAction <-
        newTeardown "bigger system"
          ((return (failedTeardownActions <> teardownActions)) :: IO [Teardown])

      toredownResult <- teardown teardownAction
      replicateM_ 9 (teardown teardownAction)

      assertEqual "teardown action count must be correct"
                  10 (toredownCount toredownResult)

      assertEqual "failed teardown action must be correct"
                  5 (failedToredownCount toredownResult)

  , testCase "teardown with list of description and actions executes correctly" $ do
      callCountRef <- newIORef (0 :: Int)
      teardownAction <-
          newTeardown "bigger-system"
              [
                ("1" :: Text, modifyIORef callCountRef (+1))
              , ("2", modifyIORef callCountRef (+1))
              , ("3", modifyIORef callCountRef (+1))
              , ("4", modifyIORef callCountRef (+1))
              , ("5", modifyIORef callCountRef (+1))
              , ("6", panic "nope")
              , ("7", panic "nope")
              , ("8", panic "nope")
              , ("9", panic "nope")
              ]


      -- Execute multiple times to assert idempotency
      toredownResult <- teardown teardownAction
      replicateM_ 9 (teardown teardownAction)

      assertEqual "teardown action count must be correct"
                  9 (toredownCount toredownResult)

      assertEqual "failed teardown must be correct"
                  4 (failedToredownCount toredownResult)

      callCount <- readIORef callCountRef
      assertEqual "side-effects were executed despite errors on other teardown operations"
                  5 callCount
  ]
