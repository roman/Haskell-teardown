{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Teardown
import Protolude

main :: IO ()
main = do
  baruta    <- newTeardown "baruta" (return () :: IO ())
  bqto      <- newTeardown "barquisimeto" (return () :: IO ())
  colombia  <- newTeardown "colombia" (return () :: IO ())
  mexico    <- newTeardown "mexico" (return () :: IO ())

  caracas   <- newTeardown "caracas" (return [baruta] :: IO [Teardown])

  venezuela <- newTeardown "venezuelan"
                           (return [bqto, caracas] :: IO [Teardown])

  canada <- newTeardown
    "canada"
    [ ("vancouver" :: Text, return () :: IO ())
    , ("calgary"          , panic "Some Error Message")
    ]

  earth <- newTeardown
    "earth"
    (return [colombia, canada, mexico, venezuela] :: IO [Teardown])

  result <- teardown earth
  print $ renderTeardownReport result
