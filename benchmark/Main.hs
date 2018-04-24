{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import RIO

import Gauge

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
  , env
      (sequence (replicate 1000 (newTeardown "benchmark" (return () :: IO ())))
       >>= newTeardown "parent")
      (\composedTeardown ->
          bench "teardown list" (whnfIO $ runTeardown_ composedTeardown))
  ]
