# Raison d'etre

The _correct_ teardown of a system becomes a crucial matter when running
applications through GHCi while doing REPL driven development; this library
provides a stable API to manage the cleanup process of resources your
application allocates when it starts up.

One could naively implement a teardown sub-routine of an application by doing
something like the following:

```haskell
-- All functions in this example initialize hypothetical resources, the
-- idea stands that there is a way to allocate a system resource
-- using some sort of configuration record, and there is a
-- sub-routine to release those resources once the application
-- shuts down

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

The previous implementation does not address a few concerns:

* If for some reason we execute the @IO ()@ sub-routine returned by the
  @initApp@ function more than once, there is likely going to be a runtime
  exception of the "already closed resource" nature. This library ensures that
  teardown sub-routines are executed /exactly/ once, even on the scenario where
  we execute the teardown procedure multiple times.

* The teardown of sub-systems can be built and composed via the @(>>)@ operator,
  what happens if the @teardownDb@ sub-routine in the previous example throws an
  exception? Likely other resource teardown sub-routines are going to be
  affected. This library ensures that errors are isolated from every other
  resource teardown sub-routines.

* All teardown sub-routines use a description argument to keep track of what is
  being cleaned up; By requiring this, we avoid confusion around what is going
  on when shutting down an application. This library makes this documentation a
  /required/ argument when building teardown sub-routines, thus helping
  trace-ability.

* You may notice the structure of teardown sub-routines form a tree shape. This
  library provides a data structure representation of this tree that allows the
  developer to report all teardown sub-routines in hierarchy order, with other
  details around if sub-routines failed (or not).

* Also, this library keeps track how much time every teardown sub-routine takes,
  allowing the developer to learn which parts of the teardown procedure are slow
  and adequately address those on development time (e.g., Faster reload =>
  Faster development feedback loops).

By using this library, you may implement without much effort a good, reliable
and transparent strategy for application resource teardown sub-routines.
