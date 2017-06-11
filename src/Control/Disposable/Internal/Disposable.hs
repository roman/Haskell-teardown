{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
module Control.Disposable.Internal.Disposable where

import           Protolude hiding (first)

import Data.Time.Clock (NominalDiffTime, diffUTCTime, getCurrentTime)

import GHC.Generics (Generic)

import Data.IORef         (newIORef, readIORef, writeIORef, atomicModifyIORef)
import Control.Exception  (SomeException, try)

--------------------------------------------------------------------------------

type Description = Text

data DisposeResult
  = BranchResult
    {
      resultDescription :: !Description
    , resultElapsedTime :: !NominalDiffTime
    , resultDidFail     :: !Bool
    , resultListing     :: ![DisposeResult]
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

newtype Disposable
  = Disposable (IO DisposeResult)
  deriving (Generic)

class IDisposable d where
  dispose :: d -> IO DisposeResult

--------------------------------------------------------------------------------

trackExecutionTime :: IO a -> IO (NominalDiffTime, a)
trackExecutionTime routine = do
  start <- getCurrentTime
  result <- routine
  end <- getCurrentTime
  return (diffUTCTime end start, result)

emptyDisposeResult :: Description -> DisposeResult
emptyDisposeResult = EmptyResult

newDisposable :: Description -> IO () -> IO Disposable
newDisposable desc disposingAction = do
  disposeResultLock <- newIORef True
  disposeResultRef  <- newIORef Nothing
  return $ Disposable $ do
      shouldDispose <-
        atomicModifyIORef disposeResultLock
                          (\pending -> (not pending, pending))

      if shouldDispose then do
          (elapsed, disposeResult0) <- trackExecutionTime (try disposingAction)
          let
            disposeResult =
              LeafResult desc elapsed (either Just (const Nothing) disposeResult0)

          writeIORef disposeResultRef (Just disposeResult)
          return disposeResult
      else
          fromMaybe (emptyDisposeResult desc) <$> readIORef disposeResultRef

didDisposeFail :: DisposeResult -> Bool
didDisposeFail result =
  case result of
    LeafResult {} ->
      isJust (resultError result)
    BranchResult {} ->
      resultDidFail result

concatDisposables :: Description -> [Disposable] -> IO Disposable
concatDisposables desc disposables =
  return $ Disposable $ do
     disposableResults <- mapM (\(Disposable action) -> action) disposables

     let
       elapsed =
         sum $ map resultElapsedTime disposableResults

       disposeFailed =
         any didDisposeFail disposableResults

     return $ BranchResult desc elapsed disposeFailed disposableResults

emptyDisposable :: Description -> IO Disposable
emptyDisposable desc =
  return $ Disposable (return $ emptyDisposeResult desc)

--------------------------------------------------------------------------------

-- foldrDisposeResult
--   :: (Description -> Maybe SomeException -> acc -> acc)
--   -> (Description -> acc -> acc)
--   -> ([acc] -> acc)
--   -> acc
--   -> DisposeResult
--   -> acc
-- foldrDisposeResult leafStep branchStep monoidStep acc disposeResult =
--   case disposeResult of
--     LeafResult desc mErr ->
--       leafStep desc mErr acc

--     BranchResult desc innerDisposeResult ->
--       branchStep desc
--         (foldrDisposeResult leafStep branchStep monoidStep acc innerDisposeResult)

--     DisposeMonoid disposeResultList0 ->
--       let
--         disposeResultList =
--           map (foldrDisposeResult leafStep branchStep monoidStep acc) disposeResultList0
--       in
--         monoidStep disposeResultList

-- foldlDisposeResult
--   :: (acc -> Description -> Maybe SomeException -> acc)
--   -> (acc -> Description -> acc)
--   -> ([acc] -> acc)
--   -> acc
--   -> DisposeResult
--   -> acc
-- foldlDisposeResult leafStep branchStep monoidStep acc disposeResult =
--   case disposeResult of
--     LeafResult desc mErr ->
--       leafStep acc desc mErr

--     BranchResult desc innerDisposeResult ->
--       foldlDisposeResult leafStep branchStep monoidStep (branchStep acc desc) innerDisposeResult

--     DisposeMonoid disposeResultList0 ->
--       let
--         disposeResultList =
--           map (foldlDisposeResult leafStep branchStep monoidStep acc) disposeResultList0
--       in
--         monoidStep disposeResultList

-- disposableCount :: DisposeResult -> Int
-- disposableCount =
--   foldrDisposeResult (\_ _ acc -> acc + 1)
--                     (const identity)
--                     sum
--                     0

-- disposableFailedCount :: DisposeResult -> Int
-- disposableFailedCount =
--   foldrDisposeResult (\_ mErr acc -> acc + maybe 0 (const 1) mErr)
--                     (const identity)
--                     sum
--                     0

--------------------------------------------------------------------------------

-- instance Monoid DisposeResult where
--   mempty =
--     emptyDisposeResult

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

-- instance Monoid Disposable where
--   mempty =
--     Disposable mempty

--   mappend (Disposable a) (Disposable b) =
--     Disposable (a `mappend` b)

instance IDisposable Disposable where
  dispose (Disposable action) =
    action
