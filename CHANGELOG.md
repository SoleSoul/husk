# Changelog

## 0.2.0 (2026-10-18)

husk now runs the exercises of the best-known learning materials: *Learn
You a Haskell*, Hutton's *Programming in Haskell*, CIS 194, the Helsinki
Haskell MOOC and H-99 (see docs/LEARNING.md). Each was compared with GHC.

Language:
- Your own exception types (`instance Exception T`, or
  `deriving (Show, Exception)`), with `catch`, `try` and `handle` choosing
  handlers by type, and `IOException`, `ErrorCall` and `ArithException`.
- A `newtype` can derive any class of its representation type
  (GeneralizedNewtypeDeriving).
- Derived instances infer contexts like `Show (f (g a))`, and instance
  contexts can be any type over the instance's variables.
- `Int` is 64 bits and wraps around on overflow, `Int8` ... `Int32` and
  `Word` ... `Word64` are real types that wrap at their size, and `Float` is
  single precision. All behave as in GHC.
- Literate Haskell (`.lhs`) files.
- An operator can follow a `do` or `case` block that layout closes
  (`do ... <newline> <|> p`, as in Hutton's parser).

Tools:
- `husk -l FILE.hs` starts the REPL with the file loaded, and a file without
  a module header or `main` can be loaded (`:l`) as in GHCi.
- `HUSK_PATH` lists more directories to find modules in.
- `hSetEcho stdin False` and `hSetBuffering stdin NoBuffering` work on a
  terminal (for games that read single keys or hidden input).
- An uncaught `error` or `undefined` shows where it was called, like GHC's
  `CallStack` lines.
- Clearer errors: GHC's "Ambiguous occurrence" for a definition that clashes
  with an explicitly imported name (Prelude names can still be redefined),
  the operators in a precedence error, a hint for `Monoid` instances without
  `Semigroup` and `Monad` instances without `Applicative` (older books and
  courses), the instance location in "No instance nor default method", and
  a plain "not supported" for multi-parameter classes, GADTs and type
  families.
- Writing to a closed pipe (`husk prog.hs | head`) stops the program
  quietly with status 141 instead of printing an exception.

Libraries:
- New: `Test.QuickCheck`, `Control.Monad.Random`, `Control.Arrow`,
  `Data.Tree`, `Data.List.NonEmpty`, `Data.Text.Encoding`, `Data.Text.Lazy`,
  `Codec.Picture` (writing PNG images), `System.IO.Unsafe`, `GHC.Exts`
  (`sortWith`, `groupWith`).
- `sconcat` takes a `NonEmpty`, as in base.
- `x ^ n` needs only `(*)` (GHC's algorithm), so it works for types such as
  matrices that don't define `fromInteger`.
- `unsafePerformIO` returns an evaluated value.
- End-of-file errors read like GHC's (`<stdin>: hGetLine: end of file`).

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
