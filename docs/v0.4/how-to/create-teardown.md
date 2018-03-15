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
allocSQLiteConn dbName = do
  db <- open dbName
  dbT <- newTeardown "sqlite conn" (close db)
  return (db, dbT)

main :: IO ()
main = do
  (conn, connT) <- allocSQLiteConn "./my-db.sqlite"
  forever (threadDelay (1000100 :: Int) >> changes conn >>= print)
    `finally` (runTeardown connT >>= print)
```
