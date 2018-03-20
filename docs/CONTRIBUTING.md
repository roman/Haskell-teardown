# Development Notes

[![Build Status](https://travis-ci.org/roman/Haskell-teardown.svg?branch=master)](https://travis-ci.org/roman/Haskell-teardown)
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/teardown.svg)](http://packdeps.haskellers.com/feed?needle=teardown)
[![Github](https://img.shields.io/github/commits-since/roman/haskell-teardown/v0.4.0.0.svg)](https://img.shields.io/github/commits-since/roman/haskell-teardown/v0.4.0.0.svg)

This library is intended to be minimal, providing a few functions that work
reliably among many different kind of projects. If you want to contribute, Pull
Request are very welcome! Please try to follow these simple rules:

* Please create a topic branch for every separate change you make, bonus points
  if you create an issue with an acceptance criteria.
* Update the README.md file if necessary.
* Please _do not_ change the version number on your Pull Request.

## Dependencies

You'll need to install [Stack](https://github.com/commercialhaskell/stack), once installed, you can execute the `make` command and learn tasks supported in the project.

You'll need to make sure you invoke `make format` and `make lint` when pushing changes, otherwise the Pull Request builder will fail.

## Open Commit Policy

This library has an open commit bit policy: Anyone with an accepted pull request gets added as a repository collaborator.

Please create a feature branch and open a pull-request early for any new features or documentation improvements.
