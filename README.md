# husk — a lightweight Haskell

husk is a small implementation of the Haskell language that covers what you
need for everyday problem solving: algebraic data types, pattern matching,
type classes with full type inference, laziness, `do` notation, records, list
comprehensions, and a standard library with `Data.Map`, `Data.Set`,
`Data.Array`, `Text.Printf`, `Control.Monad.State` and friends.

It is about 7,000 lines of Scheme plus 5,000 lines of Haskell library code. It
runs on [Chez Scheme](https://cisco.github.io/ChezScheme/), a ~10 MB native-code
Scheme compiler, so it doesn't need GHC's gigabyte-sized toolchain.

```
$ husk
husk 0.1.1 -- a lightweight Haskell.  :? for help, :q to quit
husk> let fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
husk> take 10 fibs
[0,1,1,2,3,5,8,13,21,34]
husk> :t foldr
foldr :: Foldable c => (a -> b -> b) -> b -> c a -> b
husk> import qualified Data.Map as M
husk> M.fromListWith (+) [(c, 1) | c <- "mississippi"]
fromList [('i',4),('m',1),('p',2),('s',4)]
```

## Installing

1. Install Chez Scheme 10 (Arch: `yay -S chez-scheme`; Debian/Ubuntu:
   `apt install chezscheme`; macOS: `brew install chezscheme`). The launcher
   looks for `chez`, `scheme` or `chezscheme` on your `PATH` (or set `CHEZ=`).
2. Put `bin/husk` on your `PATH`, for example:
   `ln -s "$PWD/bin/husk" ~/.local/bin/husk`
3. Optional: install `rlwrap` (`pacman -S rlwrap`) for nicer line editing in
   the REPL. husk uses it automatically if it's there, and has a simple
   built-in line editor otherwise.

The compiler builds itself the first time you run it. Compiled library code is
cached in `~/.cache/husk`, so later runs start in about 0.1 s.

## Using it

```
husk                     start the REPL (like ghci)
husk prog.hs [args...]   compile and run prog.hs (like runghc)
husk -e 'expr'           evaluate an expression
husk --check prog.hs     typecheck only
husk --scheme prog.hs    show the generated Scheme code, then run
```

REPL commands: `:t expr` (type), `:k Type` (kind), `:i name` (info),
`:browse Module`, `:l file.hs`, `:r` (reload), `:{ ... :}` for
multi-line input, `:q`. Definitions (`f x = ...`, `data ...`, `instance ...`)
and `import` lines work directly at the prompt, as do `x <- action` lines.

A program is an ordinary Haskell module with a `main :: IO ()`. Modules you
import are looked up next to the main file (`import Utils.Parser` reads
`Utils/Parser.hs`), then in the standard library (`lib/`).

### Environment variables

| Variable | Effect |
|----------|--------|
| `CHEZ` | the Chez Scheme executable to use |
| `HUSK_MAX_HEAP_MB` | memory limit (default: half of RAM, at most 8 GB) |
| `HUSK_NO_CACHE=1` | don't use the compiled-code cache |
| `HUSK_NO_RLWRAP=1`, `HUSK_NO_EDITOR=1` | REPL without rlwrap / without the built-in line editor |
| `HUSK_TIME=1`, `HUSK_STATS=1` | print compile-phase timings / run-time and GC statistics |

## What's supported

In short: the Haskell 2010 language plus the commonly used extensions
(`LambdaCase`, `MultiWayIf`, `BangPatterns`, `ScopedTypeVariables`,
`TupleSections`, `RecordWildCards`, `NamedFieldPuns`, `OverloadedStrings`,
`DeriveFunctor`/`Foldable`/`Traversable`, `BlockArguments`) and a curated
`base` with the usual containers (`Data.Map`, `Data.Set`, `Data.Sequence`,
arrays, ...). Not supported: GADTs, type families, multi-parameter classes,
rank-N types, Template Haskell.

See [docs/LANGUAGE.md](docs/LANGUAGE.md) for the full list of language
features and library modules, and for where husk differs from GHC.
[docs/GUIDE.md](docs/GUIDE.md) is a practical guide to writing programs with
husk: one recommended way to read input, pick data structures, write loops,
and so on.

## How it works

husk compiles Haskell to Scheme and lets Chez Scheme compile that to native
code on the fly. The pipeline is lexer → layout-aware parser → renamer and
desugarer → Hindley-Milner type inference with type classes (dictionary
passing) → code generator → Chez `compile`. Laziness uses thunk records,
type classes become dictionary vectors, and `do` blocks over `IO` become plain
sequential Scheme code.

[docs/DESIGN.md](docs/DESIGN.md) explains the design and how Haskell maps
onto Scheme.

## Project layout

```
bin/husk            launcher script
src/main.ss         entry point
src/husk/           the compiler (Scheme)
  lexer.ss parser.ss rename.ss derive.ss types.ss codegen.ss strict.ss driver.ss
  runtime.ss        runtime library used by compiled programs
lib/                the standard library (Haskell)
  Husk/Base.hs      core classes, instances and functions
  Prelude.hs ...    public modules re-exporting from Husk.Base
tests/              test programs with expected output (tests/run.sh)
examples/           a few example programs
docs/               language reference and design notes
```

## Tests

```
tests/run.sh
```

runs every `tests/*.hs` and compares its output with the matching `.out`
file, and checks that each `tests/errors/*.hs` fails with the expected message.

## License

MIT — see [LICENSE](LICENSE).
