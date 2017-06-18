Change log
==========

teardown uses [Semantic Versioning][1].
The change log is available [on GitHub][2].

[1]: http://semver.org/spec/v2.0.0.html
[2]: https://github.com/roman/Haskell-teardown/libraries/teardown/CHANGELOG.md

## v0.1.0.0

> BREAKING CHANGES

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
