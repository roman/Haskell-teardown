{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
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
  (
    ITeardown (..)
  , Teardown
  , TeardownResult (..)
  , IResource (..)
  )

import Control.Teardown.Internal.Core
  (
    emptyTeardown
  , didTeardownFail
  , failedToredownCount
  , toredownCount
  )

import Control.Teardown.Internal.Printer (renderTeardownReport)
