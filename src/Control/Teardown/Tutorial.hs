{-| The /correct/ teardown of a system becomes a crucial matter when running applications through GHCi while doing REPL driven development; this library provides a stable API to manage the cleanup process of resources your application allocates when it starts up.

One could naively implement a teardown sub-routine of an application by doing something like the following:

> -- All functions in this example initialize hypothetical resources, the
> -- idea stands that there is a way to allocate a system resource
> -- using some sort of configuration record, and there is a
> -- sub-routine to release those resources once the application
> -- shuts down
>
> initDb :: Logger -> DbConnInfo -> IO (DbConn, IO ())
> initDb logger connInfo = do
>   conn <- newConn connInfo
>   return (conn, info logger "Teardown Database" >> closeConn conn)
>
> initTcpServer :: Logger -> ServerInfo -> IO (Socket, IO ())
> initTcpServer logger serverInfo = do
>   socket <- startServer serverInfo
>   return (socket, info logger "Teardown Tcp Server" >> closeSocket socket)
>
> initApp :: Logger -> DbConnInfo -> ServerInfo -> IO (IO ())
> initApp logger connInfo serverInfo = do
>   (connInfo, teardownDb) <- initDb logger connInfo
>   (serverInfo, teardownSocket) <- initTcpServer logger serverInfo
>   -- do something with connInfo and serverInfo ...
>   return (info logger "Teardown Application"
>           >> teardownDb
>           >> teardownSocket)


The previous implementation does not address a few concerns:

    * If for some reason we execute the @IO ()@ sub-routine returned by the @initApp@  function more than once, there is likely going to be a runtime exception of the "already closed resource" nature. This library ensures that teardown sub-routines are executed /exactly/ once, even on the scenario where we execute the teardown procedure multiple times.

    * The teardown of sub-systems can be built and composed via the @(>>)@  operator, what happens if the @teardownDb@ sub-routine in the previous example throws an exception? Likely other resource teardown sub-routines are going to be affected. This library ensures that errors are isolated from every other resource teardown sub-routines.

    * All teardown sub-routines use a description argument to keep track of what is being cleaned up; By requiring this, we avoid confusion around what is going on when shutting down an application. This library makes this documentation a  /required/ argument when building teardown sub-routines, thus helping trace-ability.

    * You may notice the structure of teardown sub-routines form a tree shape.   This library provides a data structure representation of this tree that allows the developer to report all teardown sub-routines in hierarchy order, with other details around if sub-routines failed (or not).

    * Also, this library keeps track how much time every teardown sub-routine takes, allowing the developer to learn which parts of the teardown procedure are slow and adequately address those on development time (e.g., Faster reload => Faster development feedback loops).

    This tutorial shows some examples on how to use the @Control.Teardown@ API and also the rationale behind the typeclasses offered by the library.
-}
module Control.Teardown.Tutorial
  (
    -- * Introduction
    -- $introduction

    -- * The 'Teardown' type
    -- $teardown_type

    -- * The 'IResource' typeclass
    -- $iresource_typeclass

    -- * The 'ITeardown' typeclass
    -- $iteardown_typeclass

    -- * The 'TeardownResult' type
    -- $teardown_result_type

  )
  where

{- $introduction

    The @teardown@ library allows creating cleanup sub-routines for the resources allocated to your application; some of the features it offers are:

    * Idempotent execution of cleanup sub-routines

    * When cleanup sub-routines fail, this library guarantees that it won't stop the cleanup process of other resources

    * On cleanup sub-routine execution, it returns a Tree data structure that  reports: what succeeded, what failed and how much time each of those
      sub-routines took to execute.

    * Functions to compose a Tree of 'Teardown' records

    With this features in place, you can ensure the best cleanup effort when reloading a complex runtime application when doing REPL driven development.

-}

{- $teardown_type

    The 'Teardown' type wraps a resource cleanup sub-routine to provides the
    features displayed in the introduction section.

    == How to create a 'Teardown'

    You need to use the 'newTeardown' function to create a 'Teardown' record
    from a cleanup @IO@ sub-routine. An example follows:

    @
initDb :: Logger -> DbConnInfo -> IO (DbConn, Teardown)
initDb logger connInfo = do
  conn <- newConn connInfo
  cleanup <- newTeardown "database connection" (closeConn conn)
  return (conn, cleanup)
    @

    In the previous example, you may notice the 'newTeardown' function receives a 'Text' as its first argument; the purpose of this parameter is to explain what is the resource we want to deallocate. Make sure to make this description as unique as possible, as it will:

    * Help you pinpoint quickly a resource that is flaky at cleanup times
    * Allow you to identify what are the resources that take the longest to clean up; this is essential to help us ensure our development feedback loop is fast when doing REPL driven development.

    The second argument of the 'newTeardown' is an @IO ()@ sub-routine that performs the so-called cleanup procedure; however, the second argument could be other types that implement the 'IResource' typeclass.

-}

{- $iresource_typeclass

    If you look up the signature of the 'newTeardown' function, you will discover the second argument may be any type that implements the 'IResource'  typeclass. This typeclass allows us to define which types represent a cleanup sub-routines. Note this __is not__ a public typeclass. Following are some of its instances.

    == @IO ()@

    This instance is the one that has been shown so far in this example.

    == @[Teardown]@

    This typeclass instance is used when creating the 'Teardown' of a resource that creates many other sub-resources that return a 'Teardown' record on their own.

    @
> initApp :: Logger -> DbConnInfo -> ServerInfo -> IO (Server, Teardown))
> initApp logger connInfo serverInfo = do
>   (conn, teardownDb) <- initDb logger connInfo
>   (server, teardownSocket) <- initTcpServer logger serverInfo conn
>   -- do something with connInfo and serverInfo ...
>   appTeardown <- newTeardown "application" [teardownDb, teardownSocket]
>   return (server, appTeardown)
    @

    == @[(Text, IO ())]@

    This typeclass instance is useful when creating a 'Teardown' of an operation which  allocates more than one resource that needs to be torn down

    @
> initWorkerThread :: IO (Worker, Teardown)
> initWorkerThread = do
>   workerQueue <- newTBQueue 100
>   workerAsync <- async $ workerLoop workerQueue
>   workerTeardown <- newTeardown "worker" [("queue", closeTBQueue workerQueue)
>                                          ,("async", cancel workerAsync)]
>   return $ (Worker workerQueue workerAsync, workerTeardown)
    @

    This typeclass instances will cover most of the use cases for building cleanup sub-routines.
-}

{- $iteardown_typeclass

    Once you build a 'Teardown' record, the next thing you can do with it besides composing it with other 'Teardown's is to execute its cleanup sub-routine.

    To do this, we use the 'teardown' function, which has the following type signature:

    @
teardown :: ITeardown teardown => teardown -> IO TeardownResult
    @

    'teardown' returns a 'TeardownResult' record which is a tree-like data structure that contains detailed information on what teardown sub-routines failed (or not) and how much time they took to perform on the resource tree you built using 'newTeardown'.

    Naturally, the 'Teardown' type implements the 'ITeardown' typeclass, Is important to note, this typeclass is a public one, meaning, you are free to extend it with your types. It is __highly__ recommended. However, you implement this typeclass on types that wrap a 'Teardown' record, otherwise, the idempotence guarantees of sub-routines cannot be secured by the library.

-}

{- $teardown_result_type

    As stated on the [ITeardown typeclass](#g:4) section, the 'TeardownResult' record reports the execution of a 'teardown' call. This library provides a few functions that get interesting information out of this record:

    * 'prettyTeardownResult' -- returns a 'Doc' that represents the Tree of resources with their outcome and performance time, you can use 'print' to print the report on the terminal. Following example output:

    @
`- ✘  Application (0.000006s)
   |`- ✓ database connection (0.000002s)
   |`- ✘ tcp server (0.000002s)
         FatalError: FatalError {msg = "Some TCP Error Message"}
   @

    * 'toredownCount' -- returns the number of cleanup sub-routines executed.

    * 'failedToredownCount' -- returns the number of cleanup sub-routines that failed with an exception

    * 'didTeardownFail' -- returns a boolean that indicates if any cleanup  sub-routine failed with an exception

    This library exports all record constructors of 'TeardownResult' so you may traverse it in any way you find useful.
-}
