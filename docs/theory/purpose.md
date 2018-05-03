# Purpose

The _correct_ teardown of a system becomes a crucial matter when running
applications through GHCi while doing REPL driven development; this library
provides a stable API to manage the cleanup process of resources your
application allocates when it runs for the first time.

This library will:


* Make execution of cleanup `IO ()` sub-routines (e.g.
  [`Database.PostgreSQL.Simple.close`](https://hackage.haskell.org/package/postgresql-simple-0.5.3.0/docs/Database-PostgreSQL-Simple.html#v:close))
  idempotent. This helps avoiding runtime exception of the "already closed
  resource" nature.

* When composing with other cleanup `IO ()` sub-routines, one can use the `(>>)`
  operator, but what happens if the sub-routine throws an exception? Likely
  other resource cleanup sub-routine is not going to be executed because of a
  runtime exception. This library allows the composition of multiple cleanup `IO
  ()` sub-routines, while also ensuring that despite failures every resource
  cleanup is executed.

* All teardown sub-routines use a description argument to keep track of what is
  being cleaned up; By requiring this description, we avoid confusion around
  what is going on when shutting down an application. This library makes this
  documentation a /required/ argument when building teardown sub-routines, thus
  helping trace-ability.

* Keeps track how much time every teardown sub-routine takes, allowing the
  developer to learn which parts of the teardown procedure are slow and
  adequately address those on development time (e.g., Faster reload => Faster
  development feedback loops).

By using this library, you may implement without much effort a good, reliable
and transparent strategy for application resource teardown sub-routines.
