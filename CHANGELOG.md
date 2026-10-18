# Changelog

## Unreleased

- Writing to a closed pipe (`husk prog.hs | head`) stops the program
  quietly with status 141 instead of printing an exception.

## 0.1.1 (2026-10-18)

Fixes and additions for following *Learn You a Haskell*, prompted by an
outside evaluation against the book.

Fixed:
- A `Foldable` instance that defines only `foldMap` no longer loops forever.
- Matching a `newtype` constructor no longer evaluates the value
  (`f (Box _) = 42` returns 42 for `f undefined`).
- Type signatures of pattern bindings (`x :: Int` with `(x, y) = ...`) are
  checked when they have no type variables or context.
- Derived `Read` instances read record syntax, so `read (show r)` works for
  records.
- IO error messages show the file and the reason
  (`todo.txt: openFile: does not exist (No such file or directory)`)
  instead of an unexpanded format string.

Added:
- `:k` in the REPL shows the kind of a type or class.
- `ZipList` in `Control.Applicative`.
- `System.Directory`, `System.IO.Error` and `System.Random`.
- `Data.ByteString`, `Data.ByteString.Char8`, `Data.ByteString.Lazy` and
  `Data.ByteString.Lazy.Char8`.
- `openTempFile` in `System.IO`; `bracket_`, `bracketOnError` and
  `onException` in `Control.Exception`.

## 0.1.0 (2026-10-18)

First release.
