Change log
==========

teardown uses [Semantic Versioning][1].
The change log is available [on GitHub][2].

[1]: http://semver.org/spec/v2.0.0.html
[2]: https://github.com/roman/Haskell-teardown/libraries/teardown/CHANGELOG.md

## V0.5.0.1

* Add language pragma to support ghc-8.6.3 and stackage nightly

## V0.5.0.0

**BREAKING CHANGES**

* Move from `ansi-wl-pprint` to `prettyprinter`
* Re-implement `Printer` using `prettyprinter` API, now `prettyTeardownResult`
  returns a `Doc` type from the `prettyprinter` library
* Add a `Pretty` instance for the `DisposeResult` type
* Add a `Display` instance for the `DisposeResult` type

## v0.4.1.0

* Ensure that all `IO ()` sub-routines on `runTeardown` get executed inside a
  `MaskedUninterruptible` masking state
* Deprecate the `[(Text, IO ())]` instance of `IResource` in favor of creating a
  teardown record per de-allocated resource.
* Replace `criterion` in favor of `gauge`
* Bump `rio` to v0.1.1.0
* Improve upon documentation

## v0.4.0.0

**BREAKING CHANGES**

* Bump from lts-9.5 to lts-11
* Replace `ITeardown` typeclass in favor of `HasTeardown`
* Replace `teardown` function in favor of `runTeardown` and `runTeardown_`
* Replace `protolude` in favor of `rio`
* Rename `renderTeardownReport` to `prettyTeardownResult`
* Remove `Control.Monad.Component` in favor of having it on its own package `componentm`

## v0.3.0.0

**BREAKING CHANGES**

* Bump from lts-9.1 to lts-9.5
* Add `Control.Monad.Component` module
* Add `Control.Teardown.Tutorial` module
* Add `IResource` instance for `[Teardown]`

## v0.2.0.0

**BREAKING CHANGES**

* Bump from lts-8.21 to lts-9.1
* Re-organize test files to support nightly (GHC-8.2)
* Drop support for lts-6 (GHC-7.10)
* Bump dependencies for `time`, `QuickCheck`, `protolude` and `doctest`
* Add NFData instance for `TeardownResult` record
* Add travisCI builder for nightly

## v0.1.0.1

* Add benchmark to compare with vanilla IO unit
* Bump version of `criterion` to `1.2`

## v0.1.0.0

**BREAKING CHANGES**

* Relax Glob dependency bounds
* Add `IResource` typeclass and make `newTeardown` part of it
* Remove `concatTeardown` and `newDynTeardown` functions in favor of
  overloades of `IResource`
* Update TestSuite
* Update Example

## v0.0.0.2

* Add haddock documentation to modules

## v0.0.0.1

* First release of teardown library
