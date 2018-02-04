{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ComponentTest where

import RIO hiding (fail)

import Control.Exception  (ErrorCall (..))
import Control.Monad.Fail (fail)
import Test.Tasty         (TestTree, testGroup)
import Test.Tasty.HUnit   (assertEqual, assertFailure, testCase)

import qualified Control.Monad.Component as SUT


tests :: TestTree
tests = testGroup
  "component monad"
  [ testCase "execute aggregation of components' teardown action works" $ do
    callCountRef <- newIORef (0 :: Int)

    let componentOne = SUT.buildComponentWithCleanup
          $ return ((), ("one", modifyIORef callCountRef (+ 1)))

        componentTwo = SUT.buildComponentWithTeardown $ do
          t <- SUT.newTeardown "two" (modifyIORef callCountRef (+ 1) :: IO ())
          return ((), t)

        componentThree  = SUT.buildComponent $ return ()

        componentAction = do
          componentOne
          componentTwo
          componentThree

    result <- SUT.runComponentM "test application" componentAction
    SUT.runTeardown_ result
    callCount <- readIORef callCountRef
    assertEqual "teardown action got called more than once" 2 callCount
  , testCase "component construction failure tears down previous components"
    $ do
        callCountRef <- newIORef (0 :: Int)

        let
          componentOne = SUT.buildComponentWithCleanup
            $ return ((), ("one", modifyIORef callCountRef (+ 1)))

          componentTwo =
            SUT.buildComponent $ throwIO (ErrorCall "failing hard")

          componentThree = SUT.buildComponentWithTeardown $ do
            t <- SUT.newTeardown "three"
                                 (modifyIORef callCountRef (+ 1) :: IO ())
            return ((), t)

          componentAction = do
            componentOne
            void componentTwo
            componentThree

        result <- try $ SUT.runComponentM "test application" componentAction
        case result of
          Left (SUT.ComponentStartupFailure [err]) -> do
            callCount <- readIORef callCountRef
            assertEqual "expected introduced error, got different one"
                        (Just $ ErrorCall "failing hard")
                        (fromException err)
            assertEqual "teardown action got called more than once" 1 callCount

          Left (err :: SUT.ComponentError) ->
            assertFailure
              $  "expected SUT.ComponentStartupFailure exception, got instead: "
              <> show err

          Right _ -> assertFailure "expected error, but did not happen"
  , testCase
      "component construction multiple failure reported when using Applicative"
    $ do
        callCountRef <- newIORef (0 :: Int)

        let
          componentOne = SUT.buildComponentWithCleanup
            $ return ((), ("one", modifyIORef callCountRef (+ 1)))

          componentTwo = SUT.buildComponent $ throwIO (ErrorCall "failing two")

          componentThree =
            SUT.buildComponent $ throwIO (ErrorCall "failing three")

          componentAction = componentOne *> componentTwo *> componentThree

        result <- try $ SUT.runComponentM "test application" componentAction
        case result of
          Left (SUT.ComponentStartupFailure [errTwo, errThree]) -> do
            callCount <- readIORef callCountRef
            assertEqual "expected introduced error, got different one"
                        (Just $ ErrorCall "failing two")
                        (fromException errTwo)
            assertEqual "expected introduced error, got different one"
                        (Just $ ErrorCall "failing three")
                        (fromException errThree)
            assertEqual "teardown action got called more than once" 1 callCount

          Left (err :: SUT.ComponentError) ->
            assertFailure
              $  "expected SUT.ComponentStartupFailure exception, got instead: "
              <> show err

          Right _ -> assertFailure "expected error, but did not happen"
  , testCase "component construction allows MonadThrow calls" $ do
    callCountRef <- newIORef (0 :: Int)

    let componentOne = SUT.buildComponentWithCleanup
          $ return ((), ("one", modifyIORef callCountRef (+ 1)))

        componentTwo = SUT.buildComponentWithCleanup
          $ return ((), ("two", modifyIORef callCountRef (+ 1)))

        componentAction = do
          componentOne
          void $ throwM (ErrorCall "failing via MonadThrow")
          componentTwo

    result <- try $ SUT.runComponentM "test application" componentAction
    case result of
      Left (SUT.ComponentStartupFailure [err]) -> do
        callCount <- readIORef callCountRef
        assertEqual "expected introduced error, got different one"
                    (Just $ ErrorCall "failing via MonadThrow")
                    (fromException err)
        assertEqual "teardown action got called more than once" 1 callCount

      Left (err :: SUT.ComponentError) ->
        assertFailure
          $  "expected SUT.ComponentStartupFailure exception, got instead: "
          <> show err

      Right _ -> assertFailure "expected error, but did not happen"
  , testCase "component construction allows fail calls" $ do
    callCountRef <- newIORef (0 :: Int)

    let componentOne = SUT.buildComponentWithCleanup
          $ return ((), ("one", modifyIORef callCountRef (+ 1)))

        componentTwo = SUT.buildComponentWithCleanup
          $ return ((), ("two", modifyIORef callCountRef (+ 1)))

        componentAction = do
          void componentOne
          void $ fail "failing via fail"
          componentTwo

    result <- try $ SUT.runComponentM "test application" componentAction
    case result of
      Left (SUT.ComponentStartupFailure [err]) -> do
        callCount <- readIORef callCountRef
        assertEqual
          ("expected introduced error, got different one: " <> show err)
          "failing via fail"
          ( maybe
            ""
            ( \res -> case res of
              SUT.ComponentFailure errMsg -> errMsg
              _                           -> "FAIL - Invalid ComponentException received"
            )
            (fromException err)
          )
        assertEqual "teardown action got called more than once" 1 callCount

      Left (err :: SUT.ComponentError) ->
        assertFailure
          $  "expected SUT.ComponentStartupFailure exception, got instead: "
          <> show err

      Right _ -> assertFailure "expected error, but did not happen"
  ]
