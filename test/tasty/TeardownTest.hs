{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module TeardownTest where

import RIO

import           Control.Monad    (replicateM)
import qualified Control.Teardown as SUT
import           Test.Tasty       (TestTree, testGroup)
import           Test.Tasty.HUnit


tests :: TestTree
tests = testGroup
  "teardown"
  [ testCase "idempotent execution of teardown action" $ do
    callCountRef   <- newIORef (0 :: Int)
    teardownAction <- SUT.newTeardown
      "test cleanup"
      (atomicModifyIORef callCountRef (\a -> (a + 1, ())) :: IO ())

    replicateM_ 10 (SUT.runTeardown teardownAction)
    callCount <- readIORef callCountRef
    assertEqual "teardown action got called more than once" 1 callCount
  , testCase "failing teardown action does not stop execution" $ do
    teardownAction <- SUT.newTeardown "failing teardown"
                                      (error "failing teardown" :: IO ())

    result <- SUT.runTeardown teardownAction
    replicateM_ 9 (SUT.runTeardown teardownAction)

    assertBool  "result should report an error" (SUT.didTeardownFail result)
  , testCase "thread safe idempotent execution of teardown action" $ do
    callCountRef   <- newIORef (0 :: Int)
    teardownAction <- SUT.newTeardown
      "test cleanup"
      (atomicModifyIORef callCountRef (\a -> (a + 1, ())) :: IO ())

    asyncList <- replicateM
      10
      (async
                      -- each async executes teardown 3 times
             $ replicateM_ 3 $ SUT.runTeardown_ teardownAction)

    mapM_ wait asyncList
    callCount <- readIORef callCountRef
    assertEqual "teardown action must not be called more than once" 1 callCount
  , testCase "teardown tree keeps idempotent guarantees around execution" $ do
    callCountRefs  <- replicateM 10 $ newIORef (0 :: Int)

    teardownAction <-
      SUT.newTeardown "bigger system" $ forM callCountRefs $ \callCountRef ->
        SUT.newTeardown
          "test cleanup"
          (atomicModifyIORef callCountRef (\a -> (a + 1, ())) :: IO ())

    replicateM_ 10 (SUT.runTeardown_ teardownAction)

    countRefs <- mapM readIORef callCountRefs
    assertEqual "teardown action must not be called more than once"
                (replicate 10 1)
                countRefs
  , testCase "teardown action that returns Teardown list returns correct count"
    $ do
        failedTeardownActions <- replicateM
          5
          (SUT.newTeardown "test cleanup with failures" (error "nope" :: IO ()))

        teardownActions <- replicateM
          5
          (SUT.newTeardown "test cleanup" (return () :: IO ()))

        teardownAction <- SUT.newTeardown
          "bigger system"
          ( return (failedTeardownActions <> teardownActions) :: IO
              [SUT.Teardown]
          )

        toredownResult <- SUT.runTeardown teardownAction
        replicateM_ 9 (SUT.runTeardown teardownAction)

        assertEqual "teardown action count must be correct"
                    10
                    (SUT.toredownCount toredownResult)

        assertEqual "failed teardown action must be correct"
                    5
                    (SUT.failedToredownCount toredownResult)
  , testCase "teardown with list of description and actions executes correctly"
    $ do
        callCountRef   <- newIORef (0 :: Int)
        teardownAction <- SUT.newTeardown
          "bigger-system"
          [ ("1" :: Text, modifyIORef callCountRef (+ 1) :: IO ())
          , ("2"        , modifyIORef callCountRef (+ 1))
          , ("3"        , modifyIORef callCountRef (+ 1))
          , ("4"        , modifyIORef callCountRef (+ 1))
          , ("5"        , modifyIORef callCountRef (+ 1))
          , ("6"        , error "nope")
          , ("7"        , error "nope")
          , ("8"        , error "nope")
          , ("9"        , error "nope")
          ]


        -- Execute multiple times to assert idempotency
        toredownResult <- SUT.runTeardown teardownAction
        replicateM_ 9 (SUT.runTeardown teardownAction)

        assertEqual "teardown action count must be correct"
                    9
                    (SUT.toredownCount toredownResult)

        assertEqual "failed teardown must be correct"
                    4
                    (SUT.failedToredownCount toredownResult)

        callCount <- readIORef callCountRef
        assertEqual
          "side-effects were executed despite errors on other teardown operations"
          5
          callCount
  ]
