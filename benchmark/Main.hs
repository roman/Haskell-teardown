{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import RIO

import Criterion
import Criterion.Main

import Control.Teardown (newTeardown, runTeardown_)

main :: IO ()
main = defaultMain
  [ bgroup
      "simple IO unit return"
      [ bench "without teardown" (whnfIO $ return ())
      , env
        (newTeardown "benchmark" (return () :: IO ()))
        (\unitTeardown ->
          bench "with teardown" (whnfIO $ runTeardown_ unitTeardown)
        )
      ]
  ]
