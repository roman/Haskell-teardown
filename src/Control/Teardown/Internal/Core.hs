{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Control.Teardown.Internal.Core
  ( emptyTeardown

  , didTeardownFail
  , failedToredownCount
  , toredownCount

  , runTeardown
  , runTeardown_
  )
where

import RIO

import RIO.Time (NominalDiffTime, diffUTCTime, getCurrentTime)

import Control.Teardown.Internal.Types

--------------------------------------------------------------------------------

-- | Track duration time of the execution of an IO sub-routine
trackExecutionTime :: IO a -> IO (NominalDiffTime, a)
trackExecutionTime routine = do
  start  <- getCurrentTime
  result <- routine
  end    <- getCurrentTime
  return (diffUTCTime end start, result)

-- | Defines a Teardown that does not have any sub-routines associated to it.
emptyTeardownResult :: Description -> TeardownResult
emptyTeardownResult = EmptyResult

-- | Returns a boolean indicating if any of the cleanup sub-routine failed
didTeardownFail :: TeardownResult -> Bool
didTeardownFail result = case result of
  LeafResult{}   -> isJust (resultError result)

  BranchResult{} -> resultDidFail result

  EmptyResult{}  -> False

-- | Creates a new "Teardown" sub-routine from a cleanup "IO" action, the
-- side-effects from this action are guaranteed to be executed only once, and
-- also it is guaranteed to be thread-safe in the scenario of multiple threads
-- executing the same teardown procedure.
newTeardownIO :: Description -> IO () -> IO Teardown
newTeardownIO desc disposingAction = do
  teardownResultLock <- newIORef False
  teardownResultRef  <- newIORef Nothing
  return $ Teardown $ do
    shouldExecute <- atomicModifyIORef
      teardownResultLock
      (\toredown -> if toredown then (True, False) else (True, True))

    if shouldExecute
      then do
        (elapsed, disposeResult0) <- trackExecutionTime (try disposingAction)
        let disposeResult = LeafResult
              desc
              elapsed
              (either Just (const Nothing) disposeResult0)

        writeIORef teardownResultRef (Just disposeResult)
        return disposeResult
      else fromMaybe (emptyTeardownResult desc) <$> readIORef teardownResultRef

-- | Creates a "Teardown" sub-routine that is composed of other smaller
-- sub-routines. This is ideal for composing the cleanup of an application from
-- smaller resources allocations that are known beforehand.
concatTeardown :: Description -> [Teardown] -> Teardown
concatTeardown desc teardownChildren = Teardown $ do
  teardownResults <- mapM (\(Teardown action) -> action) teardownChildren

  let elapsed        = sum $ map resultElapsedTime teardownResults

      teardownFailed = any didTeardownFail teardownResults

  return $ BranchResult desc elapsed teardownFailed teardownResults

-- | Creates a "Teardown" sub-routine that is composed of inner sub-routines
--  that are allocated at runtime. This is useful if allocations are being
--  created and being hold on a Mutable variable of some sort (e.g. "IORef",
--  "TVar", etc) so that on cleanup this Mutable variable is read and the
--  results of the teardown operation are returned.
newDynTeardown :: Description -> IO [TeardownResult] -> Teardown
newDynTeardown desc action = Teardown $ do
  teardownResults <- action

  let elapsed        = sum $ map resultElapsedTime teardownResults

      teardownFailed = any didTeardownFail teardownResults

  return $ BranchResult desc elapsed teardownFailed teardownResults

-- | Creates a stub "Teardown" sub-routine, normally used when a contract
--  expects a teardown return but there is no allocation being made
emptyTeardown :: Description -> Teardown
emptyTeardown desc = Teardown (return $ emptyTeardownResult desc)

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
    EmptyResult desc       -> leafStep acc desc Nothing

    LeafResult desc _ mErr -> leafStep acc desc mErr

    BranchResult desc _ _ results ->
      let result = map (foldTeardownResult leafStep branchStep acc) results
      in  branchStep result desc

-- | Returns number of sub-routines executed at teardown
toredownCount :: TeardownResult -> Int
toredownCount =
  foldTeardownResult (\acc _ _ -> acc + 1) (\results _ -> sum results) 0

-- | Returns number of sub-routines that threw an exception on execution of
-- teardown
failedToredownCount :: TeardownResult -> Int
failedToredownCount = foldTeardownResult
  (\acc _ mErr -> acc + maybe 0 (const 1) mErr)
  (\results _ -> sum results)
  0

--------------------------------------------------------------------------------

instance HasTeardown Teardown where
  getTeardown = id

-- | Creates a Teardown record from a simple `IO ()` sub-routine
instance IResource (IO ()) where
  newTeardown =
    newTeardownIO

-- | Creates a Teardown record from a simple list of cleanup sub-routines
-- (creating a Teardown record for each), ideal when one single component has
-- many resources allocated and need to be cleaned out all at once.
--
-- NOTE: The @IO ()@ sub-routines given are going to be executed in reverse
-- order at teardown time.
--
instance IResource [(Text, IO ())] where
  newTeardown desc actionList = do
    teardownList <- mapM (uncurry newTeardown) actionList
    return $ concatTeardown desc teardownList

-- | Wraps a "Teardown" record; the new record will have one extra level of
-- description.
--
instance IResource Teardown where
  newTeardown desc =
    return . concatTeardown desc . return

-- | Wraps a list of "Teardown" record; the new record will have one extra level
-- of description. Same behaviour as the @[(Text, IO ())]@ instance, but works
-- for APIs that already return a "Teardown" as their cleanup.
--
instance IResource [Teardown] where
  newTeardown desc =
    return . concatTeardown desc

-- | Wraps an IO action that returns a list of "Teardown" record; the new record
-- will have one extra level of description. Same behaviour as the @[(Text, IO
-- ())]@ instance, but works for APIs that already return a "Teardown" as their
-- cleanup.
--
instance IResource (IO [Teardown]) where
  newTeardown desc getTeardownList = do
    teardownList <- getTeardownList
    return $ concatTeardown desc teardownList

-- | Creates a "Teardown" sub-routine that is composed of inner sub-routines
--  that are allocated at runtime. This is useful if allocations are being
--  created and being hold on a Mutable variable of some sort (e.g. "IORef",
--  "TVar", etc) so that on teardown time this Mutable variable is read and
--  executed and the results are returned.
instance IResource (IO [TeardownResult]) where
  newTeardown desc =
    return . newDynTeardown desc

--------------------------------------------------------------------------------

-- | Executes all composed "Teardown" sub-routines safely, and returns a Tree
-- data structure wich can be used to gather facts from the cleanup process.
runTeardown :: HasTeardown t => t -> IO TeardownResult
runTeardown t0 =
  let (Teardown teardownAction) = getTeardown t0 in teardownAction

-- | Executes all composed "Teardown" sub-routines safely.
runTeardown_ :: HasTeardown t => t -> IO ()
runTeardown_ = void . runTeardown
