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

-- foldrTeardownResult
--   :: (Description -> Maybe SomeException -> acc -> acc)
--   -> (Description -> acc -> acc)
--   -> ([acc] -> acc)
--   -> acc
--   -> TeardownResult
--   -> acc
-- foldrTeardownResult leafStep branchStep monoidStep acc disposeResult =
--   case disposeResult of
--     LeafResult desc mErr ->
--       leafStep desc mErr acc

--     BranchResult desc innerDisposeResult ->
--       branchStep desc
--         (foldrTeardownResult leafStep branchStep monoidStep acc innerDisposeResult)

--     DisposeMonoid disposeResultList0 ->
--       let
--         disposeResultList =
--           map (foldrTeardownResult leafStep branchStep monoidStep acc) disposeResultList0
--       in
--         monoidStep disposeResultList

-- foldlTeardownResult
--   :: (acc -> Description -> Maybe SomeException -> acc)
--   -> (acc -> Description -> acc)
--   -> ([acc] -> acc)
--   -> acc
--   -> TeardownResult
--   -> acc
-- foldlTeardownResult leafStep branchStep monoidStep acc disposeResult =
--   case disposeResult of
--     LeafResult desc mErr ->
--       leafStep acc desc mErr

--     BranchResult desc innerDisposeResult ->
--       foldlTeardownResult leafStep branchStep monoidStep (branchStep acc desc) innerDisposeResult

--     DisposeMonoid disposeResultList0 ->
--       let
--         disposeResultList =
--           map (foldlTeardownResult leafStep branchStep monoidStep acc) disposeResultList0
--       in
--         monoidStep disposeResultList

-- resourceCount :: TeardownResult -> Int
-- disposableCount =
--   foldrTeardownResult (\_ _ acc -> acc + 1)
--                     (const identity)
--                     sum
--                     0

-- failedTeardownCount :: TeardownResult -> Int
-- disposableFailedCount =
--   foldrTeardownResult (\_ mErr acc -> acc + maybe 0 (const 1) mErr)
--                     (const identity)
--                     sum
--                     0

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
