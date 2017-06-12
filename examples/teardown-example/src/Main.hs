{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Protolude
import Control.Teardown

main :: IO ()
main = do
  baruta    <- newTeardown "baruta" (return ())
  bqto      <- newTeardown "barquisimeto" (return ())
  new_west  <- newTeardown "new westminster" (return ())
  calgary   <- newTeardown "calgary" (panic "Some Error Message" >> return ())
  colombia  <- newTeardown "colombia" (return ())
  mexico    <- newTeardown "mexico" (return ())

  let
    csc =
      concatTeardown "caracas" [baruta]

    venezuela =
      concatTeardown "venezuela" [bqto, csc]

    vancouver =
      concatTeardown "vancouver" [new_west]

    canada =
      concatTeardown "canada" [ vancouver, calgary ]

    earth =
      concatTeardown "earth" [ colombia, canada, mexico, venezuela ]

  result <- teardown earth
  print $ renderTeardownReport result
