{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import RIO

#if MIN_VERSION_gauge(0,2,0)
import Gauge
#else
import Gauge
import Gauge.Main (defaultMain)
#endif

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
