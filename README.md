# 🗑️  teardown

> Composable, idempotent & transparent application resource cleanup sub-routines

## Table Of Contents

* [Installation](#installation)
* [Documentation](#documentation)
* [Development](#development)

## Installation

[![Hackage](https://img.shields.io/hackage/v/teardown.svg)](https://img.shields.io/hackage/v/teardown.svg)
[![Stackage LTS](https://www.stackage.org/package/teardown/badge/lts)](http://stackage.org/lts/package/teardown)
[![Stackage Nightly](https://www.stackage.org/package/teardown/badge/nightly)](http://stackage.org/nightly/package/teardown)

Make sure you include the following entry on your [cabal file's
dependecies](https://www.haskell.org/cabal/users-guide/developing-packages.html#build-information)
section.

```cabal
library:
  build-depends: teardown
```

Or on your `package.yaml`

```
dependencies:
- teardown
```

## Documentation

* [Tutorial](https://romanandreg.gitbooks.io/teardown/v0.4/tutorial.html)
* [How-To Guides](https://romanandreg.gitbooks.io/teardown/v0.4/how-to/create-teardown.html)
* [API Reference](https://hackage.haskell.org/package/teardown)

## Development

[![Build Status](https://travis-ci.org/roman/Haskell-teardown.svg?branch=master)](https://travis-ci.org/roman/Haskell-teardown)
[![Github](https://img.shields.io/github/commits-since/roman/haskell-teardown/v0.1.0.1.svg)](https://img.shields.io/github/commits-since/roman/haskell-teardown/v0.1.0.1.svg)
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/teardown.svg)](http://packdeps.haskellers.com/feed?needle=teardown)

Follow the [developer guidelines](https://romanandreg.gitbooks.io/teardown/content/CONTRIBUTING.html)

## License

Copyright (c) 2017, 2018 Roman Gonzalez

Permission to use, copy, modify, and/or distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
