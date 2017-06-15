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
  ( ITeardown
  -- * Cleanup main type and function
  , Teardown
  , teardown

  -- * Functions to create a 'Teardown' record
  , emptyTeardown
  , newTeardown
  , newDynTeardown
  , concatTeardown

  -- * Functions to deal with results from 'teardown' call
  , TeardownResult (..)
  , didTeardownFail
  , failedToredownCount
  , toredownCount
  , renderTeardownReport
  ) where

import Control.Teardown.Internal.Core
    ( ITeardown
    , Teardown
    , teardown

    , emptyTeardown
    , newTeardown
    , newDynTeardown
    , concatTeardown

    , TeardownResult (..)
    , didTeardownFail
    , failedToredownCount
    , toredownCount
    )

import Control.Teardown.Internal.Printer (renderTeardownReport)
