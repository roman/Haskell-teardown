{-# LANGUAGE NoImplicitPrelude #-}
module Control.Monad.Component.Internal.MainHelper (appMain, replMain) where

import Protolude

import qualified Data.IORef    as IORef
import qualified Foreign.Store as Store

import Control.Monad.Component.Internal.Core  (runComponentM)
import Control.Monad.Component.Internal.Types
import Control.Teardown                       (teardown)

appComponentStorePointer :: Word32
appComponentStorePointer = 777

appMain :: Text -> ComponentM a -> (a -> IO ()) -> IO ()
appMain appName componentAction mainAction = do
  appComponent <- runComponentM appName componentAction
  mainAction (fromComponent appComponent)
  void $ teardown appComponent

replMain :: Text -> ComponentM a -> (a -> IO ()) -> IO ()
replMain appName componentAction mainAction = do
  mAppComponentRefStore <- Store.lookupStore appComponentStorePointer
  case mAppComponentRefStore of
    Nothing -> do
      appComponent <- runComponentM appName componentAction
      appRuntime <- async $ mainAction (fromComponent appComponent)
      appComponentRef  <- IORef.newIORef (appComponent, appRuntime)
      _appActionsStore <- Store.newStore appComponentRef
      return ()

    Just appComponentRefStore -> do
      appComponentRef <- Store.readStore appComponentRefStore
      (oldAppComponent, oldAppRuntime) <- IORef.readIORef appComponentRef
      void $ teardown oldAppComponent
      void $ waitCatch oldAppRuntime

      appComponent <- runComponentM appName componentAction
      appRuntime <- async $ mainAction (fromComponent appComponent)
      IORef.writeIORef appComponentRef (appComponent, appRuntime)
