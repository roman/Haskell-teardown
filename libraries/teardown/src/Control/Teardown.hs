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
    ITeardown
  , IResource

  -- * Cleanup main type and function
  , Teardown
  , TeardownResult (..)
  , teardown

  -- * Functions to create a 'Teardown' record
  , emptyTeardown
  , newTeardown

  -- * Functions to deal with results from 'teardown' call
  , didTeardownFail
  , failedToredownCount
  , toredownCount
  , renderTeardownReport
  ) where

import Control.Teardown.Internal.Types
    (IResource (..), ITeardown (..), Teardown, TeardownResult (..))

import Control.Teardown.Internal.Core
    (didTeardownFail, emptyTeardown, failedToredownCount, toredownCount)

import Control.Teardown.Internal.Printer (renderTeardownReport)
