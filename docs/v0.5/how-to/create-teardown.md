# Create a Teardown

## Description

The following example:

* Creates a `Teardown` for a SQLite connection
* Executes the `Teardown` sub-routine in the end

## Code

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Teardown.HowTo.Create where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Exception (finally)
import Control.Teardown (Teardown, newTeardown, runTeardown)
import Database.SQLite.Simple (Connection, close, open, changes)

allocSQLiteConn :: String -> IO (Connection, Teardown)
allocSQLiteConn dbName = mask_ $ do
  conn <- open dbName
  connT <- newTeardown "sqlite conn" (close conn)
  return (conn, connT)

main :: IO ()
main = do
  bracket (allocSQLiteConn "./my-db.sqlite")
          (\(_, connT) -> runTeardown connT >>= print)
          (\(conn, _) ->
            forever $ do
              threadDelay (1000100 :: Int)
              result <- changes conn
              print result
          )

```
