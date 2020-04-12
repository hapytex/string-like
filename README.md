# string-like
[![Build Status of the package by Travis](https://travis-ci.com/hapytex/string-like.svg?branch=master)](https://travis-ci.com/hapytex/string-like)
[![Build Status of the package by Hackage](https://matrix.hackage.haskell.org/api/v2/packages/string-like/badge)](https://matrix.hackage.haskell.org/#/package/string-like)
[![Hackage version badge](https://img.shields.io/hackage/v/string-like.svg)](https://hackage.haskell.org/package/string-like)

This package is based on the [**`Text.StringLike`** module](https://hackage.haskell.org/package/tagsoup/docs/Text-StringLike.html) of the [**`tagsoup`** package](https://hackage.haskell.org/package/tagsoup).

The package defines a typeclass that can be implemented to provide a uniform interface for `String`-like objects.

The typeclass itself has default implementations that convert the `StringLike`
item first to a lazy `Text`, then performs the operation, and converts results back to
its `StringLike` object. This is usually /not/ as efficient as an operation for
that specific type. Therefore it is advisable to implement the other functions as well.

One can however decide to only implement `fromText` and `toText`; or `toString`.

## Contribute

You can contribute by making a pull request on the [*GitHub
repository*](https://github.com/hapytex/string-like).

You can contact the package maintainer by sending a mail to
[`hapytexeu+gh@gmail.com`](mailto:hapytexeu+gh@gmail.com).

