{-# LANGUAGE NoImplicitPrelude #-}
{-|
Module      : Control.Teardown
Description : Build composable, idempotent & transparent application cleanup sub-routines
Copyright   : (c) Roman Gonzalez, 2017
License     : MIT
Maintainer  : romanandreg@gmail.com
Stability   : experimental

Provides functions that help on the creation of Application teardown
sub-routines
-}
module Control.Teardown
  (
  -- * Typeclasses for extending teardown functionality
    HasTeardown(..)
  , IResource

  -- * Cleanup main type and function
  , Teardown
  , TeardownResult (..)
  , runTeardown
  , runTeardown_

  -- * Functions to create a 'Teardown' record
  , emptyTeardown
  , newTeardown

  -- * Functions to deal with results from 'teardown' call
  , didTeardownFail
  , failedToredownCount
  , toredownCount
  , prettyTeardownResult
  ) where

import Control.Teardown.Internal.Types
    (HasTeardown (..), IResource (..), Teardown, TeardownResult (..))

import Control.Teardown.Internal.Core
    ( didTeardownFail
    , emptyTeardown
    , failedToredownCount
    , runTeardown
    , runTeardown_
    , toredownCount
    )

import Control.Teardown.Internal.Printer (prettyTeardownResult)
