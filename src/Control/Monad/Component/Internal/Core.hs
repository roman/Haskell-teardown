{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Control.Monad.Component.Internal.Core where

import Protolude

import Control.Monad.Component.Internal.Types
import Control.Teardown                       (Teardown, newTeardown, teardown)

--------------------------------------------------------------------------------

-- | Given the name and a `ComponentM` sub-routine, this function builds an `IO`
-- sub-routine that returns a `Component` record.
--
-- The name argument is used for trace-ability purposes when executing the
-- `teardown` of a resulting `Component`.
--
-- * A note on error scenarios:
--
-- Sometimes the given `ComponentM` sub-routine may fail on execution, in such
-- cases, this function will teardown all component resources allocated so far
-- and throw a `ComponentStartupFailure` exception.
--
runComponentM :: Text -> ComponentM a -> IO (Component a)
runComponentM !appName (ComponentM ma) = do
  eResult <- ma
  case eResult of
    Left (errList, cleanupActions) -> do
      appTeardown <- newTeardown appName cleanupActions
      -- Cleanup resources allocated so far and throw error
      -- list
      void $ teardown appTeardown
      throwIO (ComponentStartupFailure errList)

    Right (a, cleanupActions) -> do
      appTeardown <- newTeardown appName cleanupActions
      return $! Component a appTeardown

-- | Transforms an `IO` sub-routine into a `ComponentM` sub-routine; the given
-- `IO` sub-routine must return a tuple where:
--
-- * First position represents the resource being returned from
--   the component
--
-- * Second position represents a named cleanup action that tears down
--   allocated resources to create the first element of the tuple
--
buildComponentWithCleanup :: IO (a, (Text, IO ())) -> ComponentM a
buildComponentWithCleanup !ma =
  ComponentM $ do
    (a, (desc, cleanupAction)) <- ma
    teardownAction <- newTeardown desc cleanupAction
    return $ Right (a, [teardownAction])

-- | Transforms an `IO` sub-routine into a `ComponentM` sub-routine; the given
-- `IO` sub-routine must return a tuple where:
--
-- * First position represents the resource being returned from
--   the component
--
-- * Second position represents a `Teardown` record that cleans up allocated
--   resources to create the first element of the tuple
--
buildComponentWithTeardown :: IO (a, Teardown) -> ComponentM a
buildComponentWithTeardown !ma =
  ComponentM $ (\(a, resourceTeardown) ->
                  Right (a, [resourceTeardown])) <$> ma

-- | Transforms an `IO` sub-routine into a `ComponentM` sub-routine; the given
-- `IO` sub-routine returns a resource that does not allocate any other
-- resources that would need to be cleaned up on a system shutdown.
--
buildComponent :: IO a -> ComponentM a
buildComponent !ma =
  ComponentM $ (\a -> Right (a, [])) <$> ma
