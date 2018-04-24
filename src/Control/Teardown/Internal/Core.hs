{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Control.Teardown.Internal.Core
  ( emptyTeardown

  , didTeardownFail
  , failedToredownCount
  , toredownCount

  , runTeardown
  , runTeardown_
  , newTeardown
  )
where

import RIO

import RIO.Time (NominalDiffTime, diffUTCTime, getCurrentTime)

#if MIN_VERSION_base(4,11,0)
import qualified GHC.TypeLits as Ty
#endif

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

-- | Creates a new "Teardown" record from a cleanup "IO" action, the
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

  let elapsed        = sum $ map getElapsedTime teardownResults

      teardownFailed = any didTeardownFail teardownResults

  return $ BranchResult desc elapsed teardownFailed teardownResults

newDynTeardown :: Description -> IO [TeardownResult] -> Teardown
newDynTeardown desc action = Teardown $ do
  teardownResults <- action

  let elapsed        = sum $ map getElapsedTime teardownResults

      teardownFailed = any didTeardownFail teardownResults

  return $ BranchResult desc elapsed teardownFailed teardownResults

-- | Creates a stub "Teardown" sub-routine, normally used when a contract
--  expects a teardown return but there is no allocation being made
emptyTeardown :: Description -> Teardown
emptyTeardown desc = Teardown (return $ emptyTeardownResult desc)
{-# INLINE emptyTeardown #-}

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

-- | Returns number of released resources from a "runTeardown" execution
toredownCount :: TeardownResult -> Int
toredownCount =
  foldTeardownResult (\acc _ _ -> acc + 1) (\results _ -> sum results) 0
{-# INLINE toredownCount #-}

-- | Returns number of sub-routines that threw an exception on execution of
-- "runTeardown"
failedToredownCount :: TeardownResult -> Int
failedToredownCount = foldTeardownResult
  (\acc _ mErr -> acc + maybe 0 (const 1) mErr)
  (\results _ -> sum results)
  0

--------------------------------------------------------------------------------

instance HasTeardown Teardown where
  getTeardown = id
  {-# INLINE getTeardown #-}

-- | Creates a new "Teardown" record from a cleanup "IO ()" sub-routine; the
-- Teardown API guarantees:
--
-- * The execution of given "IO ()" sub-routine happens exactly once
-- * The execution is thread-safe when multiple threads try to call "runTeardown"
--
-- IMPORTANT: The @IO ()@ sub-routine _must not_ block or take a long time; this
-- sub-routine cannot be stopped by an async exception
instance IResource (IO ()) where
  newTeardown =
    newTeardownIO
  {-# INLINE newTeardown #-}

-- | Deprecated instance that creates a Teardown record from a list of cleanup
-- sub-routines (creating a Teardown record for each).
--
-- WARNING: This function assumes you are creating many sub-resources at once;
-- this approach has a major risk of leaking resources, and that is why is
-- deprecated; execute newTeardown for every resource you allocate.
--
-- NOTE: The @IO ()@ sub-routines given are going to be executed in reverse
-- order at teardown time.
--
-- Since 0.4.1.0
#if MIN_VERSION_base(4,11,0)
instance Ty.TypeError ('Ty.Text "DEPRECATED: Execute a 'newTeardown' call per allocated resource")
  => IResource [(Text, IO ())] where
  newTeardown desc actionList =
    concatTeardown desc <$> mapM (uncurry newTeardown) actionList
#else
instance IResource [(Text, IO ())] where
  newTeardown desc actionList =
    concatTeardown desc <$> mapM (uncurry newTeardown) actionList
#endif

-- | Wraps an existing "Teardown" record; the wrapper "Teardown" record represents
-- a "parent resource" on the "TeardownResult"
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
  {-# INLINE newTeardown #-}

-- | Wraps an IO action that returns a list of "Teardown" record; the new record
-- will have one extra level of description. Same behaviour as the @[(Text, IO
-- ())]@ instance, but works for APIs that already return a "Teardown" as their
-- cleanup.
--
instance IResource (IO [Teardown]) where
  newTeardown desc getTeardownList =
    concatTeardown desc <$> getTeardownList
  {-# INLINE newTeardown #-}

-- | Creates a "Teardown" record from executing a sub-routine that releases
--  short-lived "Teardown" records. This is useful when short-lived "Teardown"
--  are accumulated on a collection inside a mutable variable (e.g. @IORef@,
--  @TVar@, etc) and we want to release them
instance IResource (IO [TeardownResult]) where
  newTeardown desc =
    return . newDynTeardown desc
  {-# INLINE newTeardown #-}

--------------------------------------------------------------------------------

-- | Executes all composed "Teardown" sub-routines safely. This version returns
-- a Tree data structure wich can be used to gather facts from the resource
-- cleanup
runTeardown :: HasTeardown t => t -> IO TeardownResult
runTeardown t0 =
  let (Teardown teardownAction) = getTeardown t0 in uninterruptibleMask_ teardownAction
{-# INLINE runTeardown #-}

-- | Executes all composed "Teardown" sub-routines safely
runTeardown_ :: HasTeardown t => t -> IO ()
runTeardown_ = void . runTeardown
{-# INLINE runTeardown_ #-}
