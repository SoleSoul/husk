# Learning Haskell with husk

husk aims to run every exercise of the best-known Haskell learning
materials. For the materials below, the exercises (with my solutions) and
the materials' own code were run with both husk and GHC 9.8, and the results
are the same.

| Material | What works |
|----------|------------|
| [Learn You a Haskell for Great Good!](https://learnyouahaskell.github.io/) | Everything, including the IO chapter's to-do program, `System.Random`, `ByteString`s and `:k` |
| Graham Hutton, *Programming in Haskell*, 2nd edition | Everything: the [book's code](http://people.cs.nott.ac.uk/pszgmh/pih.html) and the exercises of chapters 1–17 |
| [CIS 194](https://www.cis.upenn.edu/~cis1940/spring13/) (Brent Yorgey, Spring 2013) | All lectures and homework, except running homework 7's text editor (see below) |
| [Haskell MOOC](https://haskell.mooc.fi/) (University of Helsinki) | The exercises of sets 1–16 except 14b; check your answers in the REPL (see below) |
| [H-99: Ninety-Nine Haskell Problems](https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems) | Everything |

## Working with each material

**Learn You a Haskell.** Type the examples into the REPL (`husk`), or save
them in a file and load it with `:l file.hs`.

**Programming in Haskell.** Download the book's code and load a file into
the REPL: `husk -l cipher.hs`. The interactive games (`hangman`, `nim`,
`life`, `tictactoe`) run with `husk hangman.hs`, or from the REPL.

**CIS 194.** Put the homework's support files (`Log.hs`, `Parser.hs`, ...)
next to your solution. Two of them were written for an older GHC and need
the same changes with husk as with a current GHC:

- `Sized.hs` needs a `Semigroup` instance: add
  `instance Semigroup Size where (<>) = (+)`.
- `Editor.hs` (used to run the editor in homework 7, exercise 4) derives
  mtl's `MonadState`, a multi-parameter class, which husk doesn't support.
  All of homework 7's functions work; only the editor program doesn't run.

The lectures are literate Haskell (`.lhs`) files, which husk loads like
`.hs` files.

**Haskell MOOC.** Where the course says `stack ghci Set1.hs`, run
`husk -l Set1.hs`. The course's test runner (`stack runhaskell
Set1Test.hs`) uses Template Haskell, which husk doesn't support; instead,
try the examples given with each exercise in the REPL. Set 14b needs a web
server and an SQLite database (the `warp` and `sqlite-simple` packages),
which husk doesn't have. Set 8's pictures are written as PNG files as in the
course.

**H-99.** All the problems, including the ones that use random numbers.

## Other materials

Most of the core of other books works too, but these rely on parts of the
Haskell ecosystem that husk doesn't include:

- *Real World Haskell*: many chapters use the FFI, databases, networking,
  GUIs, concurrency (STM) and other Hackage packages.
- *Haskell Programming from First Principles*: the testing chapters use
  `hspec` and `checkers` (husk has `Test.QuickCheck`), the parsing chapter
  uses `trifecta`, and the last chapters use networking and databases.
- *Write Yourself a Scheme in 48 Hours* uses `parsec` and mtl's `MonadError`
  (a multi-parameter class).
- Advanced material: GADTs, type families, multi-parameter type classes,
  rank-N types and Template Haskell are not supported.

## Libraries

husk can't load packages from Hackage: real packages depend on GHC's
internals and extensions (unboxed types, the FFI, CPP, multi-parameter
classes, ...), so supporting them would mean reimplementing GHC. Instead,
husk includes its own versions of the libraries that learning materials use,
with the same module names and functions:

| Package | Modules |
|---------|---------|
| base | `Prelude`, `Data.List`, `Data.Char`, `Data.Maybe`, `Control.Monad`, `Control.Exception`, `Data.IORef`, `Text.Printf`, `Control.Arrow`, `Data.List.NonEmpty`, ... |
| containers | `Data.Map`, `Data.Set`, `Data.IntMap`, `Data.Sequence`, `Data.Tree` |
| array | `Data.Array` and the mutable arrays |
| mtl, transformers | `Control.Monad.State`, `Reader`, `Writer` and the transformer modules (without mtl's multi-parameter classes) |
| text, bytestring | `Data.Text`, `Data.Text.Encoding`, `Data.ByteString` and the lazy and `Char8` variants |
| random, MonadRandom | `System.Random`, `Control.Monad.Random` |
| QuickCheck | `Test.QuickCheck` |
| directory, split | `System.Directory`, `Data.List.Split` |
| JuicyPixels | `Codec.Picture` (creating images and writing PNG files) |

[LANGUAGE.md](LANGUAGE.md#standard-library) lists all the modules.

Your own modules are found next to the main file. For modules you use from
several places (a course's support files, say), put them in a directory and
list it in `HUSK_PATH`:

```
export HUSK_PATH=~/haskell/lib:~/courses/cis194
```

A plain Haskell module from elsewhere (one that uses only the language
features husk supports) works the same way.
