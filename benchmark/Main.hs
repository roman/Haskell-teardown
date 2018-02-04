{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Protolude

import Criterion
import Criterion.Main

import Control.Teardown (newTeardown, teardown)

main :: IO ()
main = defaultMain
  [ bgroup
      "simple IO unit return"
      [ bench "without teardown" (whnfIO $ return ())
      , env
        (newTeardown "benchmark" (return () :: IO ()))
        ( \unitTeardown ->
          bench "with teardown" (whnfIO $ void $ teardown unitTeardown)
        )
      ]
  ]
