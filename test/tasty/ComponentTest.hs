{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ComponentTest where

import Protolude

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit

import Control.Monad.Component
import Data.IORef              (modifyIORef, newIORef, readIORef)


tests :: TestTree
tests =
  testGroup "component monad"
  [
    testCase "execute aggregation of components' teardown action works" $ do
      callCountRef <- newIORef (0 :: Int)

      let
        componentOne =
          buildComponentWithCleanup $ do
            return ((), ("one", modifyIORef callCountRef succ))

        componentTwo =
          buildComponentWithTeardown $ do
            t <- newTeardown "two" (modifyIORef callCountRef succ)
            return ((), t)

        componentThree =
          buildComponent $ return ()

        componentAction = do
          componentOne
          componentTwo
          componentThree

      result <- runComponentM "test application" componentAction
      void $ teardown result
      callCount <- readIORef callCountRef
      assertEqual "teardown action got called more than once"
                  2 callCount

  , testCase "component construction failure tears down previous components" $ do
      callCountRef <- newIORef (0 :: Int)

      let
        componentOne =
          buildComponentWithCleanup $ do
            return ((), ("one", modifyIORef callCountRef succ))

        componentTwo =
          buildComponent $ throwIO (ErrorCall "failing hard")

        componentThree =
          buildComponentWithTeardown $ do
            t <- newTeardown "three" (modifyIORef callCountRef succ)
            return ((), t)

        componentAction = do
          componentOne
          void componentTwo
          componentThree

      result <- try $ runComponentM "test application" componentAction
      case result of
        Left (ComponentStartupFailure [err]) -> do
          callCount <- readIORef callCountRef
          assertEqual "expected introduced error, got different one"
                      (Just $ ErrorCall "failing hard")
                      (fromException err)
          assertEqual "teardown action got called more than once" 1 callCount

        Left (err :: ComponentError) -> do
          assertFailure $ "expected ComponentStartupFailure exception, got instead: " <> show err

        Right _ ->
          assertFailure "expected error, but did not happen"

  , testCase "component construction multiple failure reported when using Applicative" $ do
      callCountRef <- newIORef (0 :: Int)

      let
        componentOne =
          buildComponentWithCleanup $ do
            return ((), ("one", modifyIORef callCountRef succ))

        componentTwo =
          buildComponent $ throwIO (ErrorCall "failing two")

        componentThree =
          buildComponent $ throwIO (ErrorCall "failing three")

        componentAction =
          componentOne
          *> componentTwo
          *> componentThree

      result <- try $ runComponentM "test application" componentAction
      case result of
        Left (ComponentStartupFailure [errTwo, errThree]) -> do
          callCount <- readIORef callCountRef
          assertEqual "expected introduced error, got different one"
                      (Just $ ErrorCall "failing two")
                      (fromException errTwo)
          assertEqual "expected introduced error, got different one"
                      (Just $ ErrorCall "failing three")
                      (fromException errThree)
          assertEqual "teardown action got called more than once" 1 callCount

        Left (err :: ComponentError) -> do
          assertFailure $ "expected ComponentStartupFailure exception, got instead: " <> show err

        Right _ ->
          assertFailure "expected error, but did not happen"

  , testCase "component construction allows MonadThrow calls" $ do
      callCountRef <- newIORef (0 :: Int)

      let
        componentOne =
          buildComponentWithCleanup $ do
            return ((), ("one", modifyIORef callCountRef succ))

        componentTwo =
          buildComponentWithCleanup $ do
            return ((), ("two", modifyIORef callCountRef succ))

        componentAction = do
          componentOne
          void $ throwM (ErrorCall "failing via MonadThrow")
          componentTwo

      result <- try $ runComponentM "test application" componentAction
      case result of
        Left (ComponentStartupFailure [err]) -> do
          callCount <- readIORef callCountRef
          assertEqual "expected introduced error, got different one"
                      (Just $ ErrorCall "failing via MonadThrow")
                      (fromException err)
          assertEqual "teardown action got called more than once" 1 callCount

        Left (err :: ComponentError) -> do
          assertFailure $ "expected ComponentStartupFailure exception, got instead: " <> show err

        Right _ ->
          assertFailure "expected error, but did not happen"

  , testCase "component construction allows fail calls" $ do
      callCountRef <- newIORef (0 :: Int)

      let
        componentOne =
          buildComponentWithCleanup $ do
            return ((), ("one", modifyIORef callCountRef succ))

        componentTwo =
          buildComponentWithCleanup $ do
            return ((), ("two", modifyIORef callCountRef succ))

        componentAction = do
          componentOne
          void $ fail "failing via fail"
          componentTwo

      result <- try $ runComponentM "test application" componentAction
      case result of
        Left (ComponentStartupFailure [err]) -> do
          callCount <- readIORef callCountRef
          assertEqual ("expected introduced error, got different one: " <> show err)
                      "failing via fail"
                      (maybe "" (\res ->
                                   case res of
                                     ComponentFailure errMsg ->
                                       errMsg
                                     _ ->
                                       "FAIL - Invalid ComponentException received")
                                (fromException err))
          assertEqual "teardown action got called more than once" 1 callCount

        Left (err :: ComponentError) -> do
          assertFailure $ "expected ComponentStartupFailure exception, got instead: " <> show err

        Right _ ->
          assertFailure "expected error, but did not happen"
  ]
