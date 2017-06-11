{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Control.Teardown.Internal.Core where

import Protolude hiding (first)

import Data.Time.Clock (NominalDiffTime, diffUTCTime, getCurrentTime)

import GHC.Generics (Generic)

import Control.Exception (SomeException, try)
import Data.IORef        (atomicModifyIORef, newIORef, readIORef, writeIORef)

--------------------------------------------------------------------------------

type Description = Text

data TeardownResult
  = BranchResult
    {
      resultDescription :: !Description
    , resultElapsedTime :: !NominalDiffTime
    , resultDidFail     :: !Bool
    , resultListing     :: ![TeardownResult]
    }
  | LeafResult
    {
      resultDescription :: !Description
    , resultElapsedTime :: !NominalDiffTime
    , resultError       :: !(Maybe SomeException)
    }
  | EmptyResult
    {
      resultDescription :: !Description
    }
  deriving (Generic, Show)

newtype Teardown
  = Teardown (IO TeardownResult)
  deriving (Generic)

class ITeardown d where
  teardown :: d -> IO TeardownResult

--------------------------------------------------------------------------------

trackExecutionTime :: IO a -> IO (NominalDiffTime, a)
trackExecutionTime routine = do
  start <- getCurrentTime
  result <- routine
  end <- getCurrentTime
  return (diffUTCTime end start, result)

emptyTeardownResult :: Description -> TeardownResult
emptyTeardownResult = EmptyResult

didTeardownFail :: TeardownResult -> Bool
didTeardownFail result =
  case result of
    LeafResult {} ->
      isJust (resultError result)

    BranchResult {} ->
      resultDidFail result

    EmptyResult {} ->
      False

newTeardown :: Description -> IO () -> IO Teardown
newTeardown desc disposingAction = do
  teardownResultLock <- newIORef False
  teardownResultRef  <- newIORef Nothing
  return $ Teardown $ do
      shouldExecute <-
        atomicModifyIORef teardownResultLock
                          (\toredown ->
                             if toredown then
                               (True, False)
                             else
                               (True, True))

      if shouldExecute then do
          (elapsed, disposeResult0) <- trackExecutionTime (try disposingAction)
          let
            disposeResult =
              LeafResult desc elapsed (either Just (const Nothing) disposeResult0)

          writeIORef teardownResultRef (Just disposeResult)
          return disposeResult
      else
          fromMaybe (emptyTeardownResult desc) <$> readIORef teardownResultRef

concatTeardown :: Description -> [Teardown] -> IO Teardown
concatTeardown desc disposables =
  return $ Teardown $ do
     disposableResults <- mapM (\(Teardown action) -> action) disposables

     let
       elapsed =
         sum $ map resultElapsedTime disposableResults

       disposeFailed =
         any didTeardownFail disposableResults

     return $ BranchResult desc elapsed disposeFailed disposableResults

emptyTeardown :: Description -> IO Teardown
emptyTeardown desc =
  return $ Teardown (return $ emptyTeardownResult desc)

--------------------------------------------------------------------------------

foldTeardownResult
  :: (acc -> Description -> Maybe SomeException -> acc)
  -> ([acc] -> Description -> acc)
  -> acc
  -> TeardownResult
  -> acc
foldTeardownResult leafStep branchStep acc disposeResult =
  case disposeResult of
    LeafResult desc _ mErr ->
      leafStep acc desc mErr

    BranchResult desc _ _ results ->
      let
        result =
          map (foldTeardownResult leafStep branchStep acc) results
      in
        branchStep result desc

toredownCount :: TeardownResult -> Int
toredownCount =
  foldTeardownResult (\acc _ _ -> acc + 1)
                     (\results _ -> sum results)
                     0

failedToredownCount :: TeardownResult -> Int
failedToredownCount =
  foldTeardownResult (\acc _ mErr -> acc + maybe 0 (const 1) mErr)
                     (\results _ -> sum results)
                     0

--------------------------------------------------------------------------------

-- instance Monoid TeardownResult where
--   mempty =
--     emptyTeardownResult

--   mappend a b =
--     case (a, b) of
--       (DisposeMonoid as, DisposeMonoid bs) ->
--         DisposeMonoid (as ++ bs)

--       (DisposeMonoid as, _) ->
--         DisposeMonoid (as ++ [b])

--       (_, DisposeMonoid bs) ->
--         DisposeMonoid (a:bs)

--       _ ->
--         DisposeMonoid [a, b]

-- instance Monoid Teardown where
--   mempty =
--     Teardown mempty

--   mappend (Teardown a) (Teardown b) =
--     Teardown (a `mappend` b)

instance ITeardown Teardown where
  teardown (Teardown action) =
    action
