{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Control.Monad.Component.Internal.Types where

import Protolude hiding (try)

import           Control.Exception.Safe (try)
import           Control.Monad.Catch    (MonadThrow (..))
import           Control.Monad.Fail     (MonadFail (..))
import qualified Data.Text              as T

import Control.Teardown (ITeardown (..), Teardown)

--------------------------------------------------------------------------------

data ComponentError
  = ComponentFailure !Text
  | ComponentStartupFailure ![SomeException]
  deriving (Generic, Show)

instance Exception ComponentError

-- | `ComponentM` is a wrapper of the `IO` monad that automatically deals with
-- the composition of `Teardown` sub-routines from resources allocated in every
-- resource of your application. To build `ComponentM` actions see the
-- `buildComponent`, `buildComponentWithCleanup` and
-- `buildComponentWithTeardown` functions.
newtype ComponentM a
  = ComponentM (IO (Either ([SomeException], [Teardown])
                           (a, [Teardown])))

instance Functor ComponentM where
  fmap f (ComponentM action) =
    ComponentM $ do
      result <- action
      return $! case result of
        Left err ->
          Left err
        Right (a, teardownList) ->
          Right (f a, teardownList)

instance Applicative ComponentM where
  pure a =
    ComponentM
      $ return
      $ Right (a, [])

  (ComponentM mf) <*> (ComponentM mm) = ComponentM $ do
    ef <- try mf
    em <- try mm
    case (ef, em) of
      ( Left err1, Left err2 ) ->
        return $ Left ( [err1, err2], [] )

      ( Left err1, Right (Left (err2, cs2)) ) ->
        return $ Left ( [err1] <> err2, cs2 )

      ( Left err1, Right (Right (_, cs2)) ) ->
        return $ Left ( [err1], cs2 )


      ( Right (Left (err1, cs1)), Left err2 ) ->
        return $ Left ( err1 <> [err2], cs1 )

      ( Right (Right (_, cs1)), Left err2 ) ->
        return $ Left ( [err2], cs1 )

      ( Right (Left (err, cs1)), Right (Right (_, cs2)) ) ->
        return $ Left ( err
                      , cs1 <> cs2
                      )

      ( Right (Left (err1, cs1)), Right (Left (err2, cs2)) ) ->
        return $ Left ( err1 <> err2
                      , cs1 <> cs2
                      )

      ( Right (Right (_, cs1)), Right (Left (err, cs2)) ) ->
        return $ Left ( err
                      , cs1 <> cs2
                      )

      ( Right (Right (f, cs1)), Right (Right (a, cs2)) ) ->
        return $ Right ( f a
                       , cs1 <> cs2
                       )

instance Monad ComponentM where
  return =
    pure

  (ComponentM action0) >>= f = ComponentM $ do
    eResult0 <- action0
    case eResult0 of
      Right (a, cs0) -> do
        let
          (ComponentM action1) = f a

        eResult1 <- try action1

        case eResult1 of
          -- There was an exception via the IO Monad
          Left err ->
            return $ Left ([err], cs0)

          -- There was an exception either via `fail` or `throwM`
          Right (Left (err, cs1)) ->
            return $ Left (err, cs0 <> cs1)

          Right (Right (b, cs1)) ->
            return $ Right (b, cs0 <> cs1)


      Left (err, cs0) ->
        return $ Left (err, cs0)

instance MonadFail ComponentM where
  fail str =
    ComponentM
      $ return
      $ Left ([toException $! ComponentFailure (T.pack str)], [])

instance MonadThrow ComponentM where
  throwM e =
    ComponentM
      $ return
      $ Left ([toException e], [])

instance MonadIO ComponentM where
  liftIO action = ComponentM $ do
    result <- action
    return $ Right (result, [])


-- | Represents the result of a `ComponentM` sub-routine, it contains a resource
-- record which can be recovered using `fromComponent` and a `Teardown`
-- sub-routine that can be executed using the `teardown` function.
data Component a
  = Component { componentResource :: !a
              , componentTeardown :: !Teardown }
  deriving (Generic)

-- | Fetches the resource of a `Component` returned by a `ComponentM`
-- sub-routine.
fromComponent :: Component a -> a
fromComponent =
  componentResource
{-# INLINE fromComponent #-}

instance NFData a => NFData (Component a)

instance ITeardown (Component a) where
  teardown =
    teardown . componentTeardown
