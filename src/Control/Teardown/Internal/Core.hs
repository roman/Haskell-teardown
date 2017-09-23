{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Control.Teardown.Internal.Core where

import Protolude hiding (first)

import Data.IORef      (atomicModifyIORef, newIORef, readIORef, writeIORef)
import Data.Time.Clock (NominalDiffTime, diffUTCTime, getCurrentTime)

import Control.Teardown.Internal.Types

--------------------------------------------------------------------------------

trackExecutionTime :: IO a -> IO (NominalDiffTime, a)
trackExecutionTime routine = do
  start <- getCurrentTime
  result <- routine
  end <- getCurrentTime
  return (diffUTCTime end start, result)

emptyTeardownResult :: Description -> TeardownResult
emptyTeardownResult = EmptyResult

-- | Returns a boolean indicating if any of the cleanup sub-routine failed
didTeardownFail :: TeardownResult -> Bool
didTeardownFail result =
  case result of
    LeafResult {} ->
      isJust (resultError result)

    BranchResult {} ->
      resultDidFail result

    EmptyResult {} ->
      False

-- | Creates a new "Teardown" sub-routine from a cleanup "IO" action, the
-- side-effects from this action are guaranteed to be executed only once, and
-- also it is guaranteed to be thread-safe in the scenario of multiple threads
-- executing the same teardown procedure.
newTeardownIO :: Description -> IO () -> IO Teardown
newTeardownIO desc disposingAction = do
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

-- | Creates a "Teardown" sub-routine that is composed of other smaller sub-routines. This
-- is ideal for composing the cleanup of an application from smaller resources allocations that
-- are known at compilation time.
concatTeardown :: Description -> [Teardown] -> Teardown
concatTeardown desc teardownChildren = Teardown $ do
  teardownResults <- mapM (\(Teardown action) -> action) teardownChildren

  let
    elapsed =
      sum $ map resultElapsedTime teardownResults

    teardownFailed =
      any didTeardownFail teardownResults

  return $ BranchResult desc elapsed teardownFailed teardownResults

-- | Creates a "Teardown" sub-routine that is composed of inner sub-routines
--  that are allocated at runtime. This is useful if allocations are being
--  created and being hold on a Mutable variable of some sort (e.g. "IORef",
--  "TVar", etc) so that on cleanup this Mutable variable is read and the
--  results of the teardown operation are returned.
newDynTeardown :: Description -> IO [TeardownResult] -> Teardown
newDynTeardown desc action = Teardown $ do
  teardownResults <- action

  let
    elapsed =
      sum $ map resultElapsedTime teardownResults

    teardownFailed =
      any didTeardownFail teardownResults

  return $ BranchResult desc elapsed teardownFailed teardownResults

-- | Creates a stub "Teardown" sub-routine, normally used when a contract
--  expects a teardown return but there is no allocation being made
emptyTeardown :: Description -> Teardown
emptyTeardown desc =
  Teardown (return $ emptyTeardownResult desc)

--------------------------------------------------------------------------------

-- | Aggregate the results of a "Teardown" sub-routine, having a function
--   for parent and leaf sub-routines from the teardown tree.
foldTeardownResult
  :: (acc -> Description -> Maybe SomeException -> acc) -- ^ Step function called when the "TeardownResult" is a leaf
  -> ([acc] -> Description -> acc) -- ^ Step function called when the "TeardownResult" is a branch
  -> acc -- ^ Original fold accumulator
  -> TeardownResult -- ^ Result from the "teardown" execution
  -> acc
foldTeardownResult leafStep branchStep acc disposeResult =
  case disposeResult of
    EmptyResult desc ->
      leafStep acc desc Nothing

    LeafResult desc _ mErr ->
      leafStep acc desc mErr

    BranchResult desc _ _ results ->
      let
        result =
          map (foldTeardownResult leafStep branchStep acc) results
      in
        branchStep result desc

-- | Returns number of sub-routines executed at teardown
toredownCount :: TeardownResult -> Int
toredownCount =
  foldTeardownResult (\acc _ _ -> acc + 1)
                     (\results _ -> sum results)
                     0

-- | Returns number of sub-routines that threw an exception on execution of
-- teardown
failedToredownCount :: TeardownResult -> Int
failedToredownCount =
  foldTeardownResult (\acc _ mErr -> acc + maybe 0 (const 1) mErr)
                     (\results _ -> sum results)
                     0

--------------------------------------------------------------------------------

instance ITeardown Teardown where
  teardown (Teardown action) =
    action

instance IResource (IO ()) where
  newTeardown =
    newTeardownIO

instance IResource [(Text, IO ())] where
  newTeardown desc actionList = do
    teardownList <- mapM (uncurry newTeardown) actionList
    return $ concatTeardown desc teardownList

instance IResource [Teardown] where
  newTeardown desc =
    return . concatTeardown desc

instance IResource (IO [Teardown]) where
  newTeardown desc getTeardownList = do
    teardownList <- getTeardownList
    return $ concatTeardown desc teardownList
