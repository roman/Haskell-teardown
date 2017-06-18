[![Build Status](https://travis-ci.org/roman/Haskell-teardown.svg?branch=master)](https://travis-ci.org/roman/Haskell-teardown)
[![Github](https://img.shields.io/github/commits-since/roman/haskell-teardown/v0.1.0.0.svg)](https://img.shields.io/github/commits-since/roman/haskell-teardown/v0.1.0.0.svg)
[![Hackage](https://img.shields.io/hackage/v/teardown.svg)](https://img.shields.io/hackage/v/teardown.svg)
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/teardown.svg)](https://img.shields.io/hackage/v/teardown.svg)
[![Stackage LTS](http://stackage.org/package/teardown/badge/lts)](http://stackage.org/lts/package/teardown)
[![Stackage Nightly](http://stackage.org/package/teardown/badge/nightly)](http://stackage.org/nightly/package/teardown)
# 🗑️ teardown

> Composable, idempotent & transparent application resource cleanup sub-routines

## Table Of Contents

* [Raison d'etre](#raison-detre)
* [Create a teardown sub-routine](#create-a-teardown-sub-routine)
* [Concatenate multiple teardown sub-routines](#concatenate-multiple-teardown-sub-routines)
* [Report teardown sub-routine results](#report-teardown-sub-routine-tree-with-results)

## Raison d'etre

The _correct_ teardown of a system becomes a very important matter
when running applications through GHCi while on development, this library
facilitates the teardown sub-routine building of an application.

One could naively implement a teardown sub-routine of an application by doing
something like the following:

```haskell

-- All functions used here are all from hypothetical
-- resources, the idea stands that there is a way to allocate
-- a system resource using some sort of configuration record, and
-- there is a sub-routine to close those resources

initDb :: Logger -> DbConnInfo -> IO (DbConn, IO ())
initDb logger connInfo = do
  conn <- newConn connInfo
  return (conn, info logger "Teardown Database" >> closeConn conn)

initTcpServer :: Logger -> ServerInfo -> IO (Socket, IO ())
initTcpServer logger serverInfo = do
  socket <- startServer serverInfo
  return (socket, info logger "Teardown Tcp Server" >> closeSocket socket)

initApp :: Logger -> DbConnInfo -> ServerInfo -> IO (IO ())
initApp logger connInfo serverInfo = do
  (connInfo, teardownDb) <- initDb logger connInfo
  (serverInfo, teardownSocket) <- initTcpServer logger serverInfo
  -- do something with connInfo and serverInfo ...
  return (info logger "Teardown Application"
          >> teardownDb
          >> teardownSocket)
```

The previous implementation has a few concerns:

* If for some reason the returned `IO ()` teardown sub-routine is executed more
  than once, there is likely going to be a runtime exception of the _already
  closed resource_ nature. This library ensures that teardown sub-routines are
  executed _only_ once, even on the scenario of it being called multiple times.

* Teardown of sub-systems are composed using the `(>>)` operator, what happens
  if the `teardownDb` sub-routine throws an exception? Likely other resource
  teardown sub-routines are going to be affected. This library ensures that
  errors are isolated from every other resource teardown sub-routines.

* All teardown sub-routines are using a logger to keep track of what is being
  cleaned up, this is an optional operation that could be skipped and cause
  confusion around what is going on when shutting down an application. This
  library makes this a _required_ argument when building teardown sub-routines,
  having always a description of what is being torn down;

* You may notice the structure of teardown sub-routines form a Tree. This
  library provides a data structure representation of this Tree that allows the
  developer to report all teardown sub-routines in hierarchy order, with details
  around if sub-routines failed (or not).

* In addition, this library keeps track of how much time every teardown
  sub-routine takes, allowing the developer to learn which parts of the teardown
  operations are slow so that they can effectively address those on development
  (e.g. Faster reload => Faster feedback loops).

By using this library you implement without much effort a good, reliable and
transparent strategy for application resource teardown sub-routines.

## Create a teardown sub-routine

The general use case for creating a teardown sub-routine is fulfilled by the
`newTeardown` function:

```haskell
initDb :: Logger -> DbConnInfo -> IO (DbConn, Teardown)
initDb logger connInfo = do
  conn <- newConn connInfo
  cleanup <- newTeardown "database connection" (closeConn conn)
  return (conn, cleanup)
```
## Concatenate multiple teardown sub-routines

To create teardown sub-routines from smaller teardown sub-routines, you use the
`concatTeardown` function:

```haskell
initApp :: Logger -> DbConnInfo -> ServerInfo -> IO Teardown
initApp logger connInfo serverInfo = do
  (connInfo, teardownDb) <- initDb logger connInfo
  (serverInfo, teardownSocket) <- initTcpServer logger serverInfo
  -- do something with connInfo and serverInfo ...
  return (concatTeardown "Application" [teardownDb, teardownSocket])
```

Note that `concatTeardown` will execute teardown sub-routines from left to
right, so if one teardown depends on another, you need to put the one
without dependencies first.

## Report teardown sub-routine results

To report what is the outcome of a teardown sub-routine, you can pass the result
from the execution of the `teardown` function into the `renderTeardownReport`
function.

```haskell
main :: IO ()
main = do
  logger <- newLogger INFO
  appTeardown <- initApp logger defaultConnInfo defaultTcpSettings
  -- shutdown app after 5 seconds because YOLO
  threadDelay 5000000
  teardown appTeardown >>= print . renderTeardownReport
```

This will give you an output like the following:

```text
`- ✘  Application (0.000006s)
   |`- ✓ database connection (0.000002s)
   |`- ✘ tcp server (0.000002s)
         FatalError: FatalError {msg = "Some TCP Error Message"}
```
