module Main where

import System.FilePath.Glob
import Test.DocTest

main :: IO ()
main = glob "src/**/*.hs" >>= doctest
