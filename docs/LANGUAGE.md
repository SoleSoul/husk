# The husk language

husk implements Haskell 2010 plus the extensions people use most in everyday
code. This page lists what is supported, which library modules exist, and
where husk behaves differently from GHC.

## Language features

### Supported

- **Modules**: `module M (exports) where`, `import M`, `import qualified M as N`,
  import lists, `hiding`, re-exporting with `module M` in export lists.
  Multi-module programs (modules are found next to the main file).
- **Definitions**: multiple equations with pattern matching, guards (including
  pattern guards `| Just x <- e` and `| let y = ...`), `where` clauses, local
  functions, operator definitions (`x <+> y = ...`), fixity declarations
  (`infixl 6 <+>`, also local ones), type signatures (including on several names
  at once, `f, g :: Int`).
- **Expressions**: lambdas, `let`, `if`, `case`, `do`, operator sections
  (`(+ 1)`, `(1 +)`, ``(`div` 2)``), negation, arithmetic sequences (`[1..]`,
  `[1,3..9]`, `['a'..'z']`), list comprehensions (with multiple generators,
  guards and `let`), tuples, type annotations (`e :: T`).
- **Patterns**: variables, wildcards, literals (including negative and string
  literals), constructors, infix constructors (`x:xs`), tuples, lists,
  as-patterns (`all@(x:xs)`), lazy patterns (`~(a, b)`), bang patterns (`!x`),
  record patterns (`P { name = n }`), pattern type signatures (`(e :: SomeException)`).
- **Data types**: `data` with several constructors, parameters, records (field
  selection, construction, update `r { f = x }`), strictness annotations
  (`!Int`), infix constructors (`data E = E :+: E`), `newtype`, `type` synonyms
  (with parameters).
- **Deriving**: `Eq`, `Ord`, `Show`, `Read`, `Enum`, `Bounded`, `Ix`, `Functor`,
  `Foldable`, `Traversable`; also standalone `deriving instance Show T`.
- **Type classes**: class declarations with superclasses, default methods,
  operator methods, and constructor classes (`class Container f where ...`).
  Instances can have contexts (`instance Show a => Show (Tree a)`) and be for
  partially applied types (`instance Functor (Either e)`,
  `instance Monad ((->) r)`). FlexibleInstances-style heads (`instance C (T Int)`)
  work too.
- **Type inference**: full Hindley-Milner inference with type classes,
  polymorphic local `let`/`where` bindings, the monomorphism restriction, and
  Haskell's defaulting rules (`Integer`, then `Double`).
- **Laziness**: non-strict semantics everywhere: infinite lists, knot-tying
  (`fibs = 0 : 1 : zipWith (+) fibs (tail fibs)`), lazy arrays for dynamic
  programming, lazy `getContents`. `seq`, `$!`, `foldl'` and bang patterns
  force evaluation.
- **Numbers**: `Int`, `Integer` (arbitrary precision), `Double`, `Rational`
  (`Data.Ratio`), with the standard numeric classes (`Num`, `Integral`,
  `Fractional`, `Floating`, `RealFrac`, `RealFloat`) and conversions
  (`fromIntegral`, `realToFrac`, `truncate`, `round`, ...).

### Extensions

These can be used without (or with) a `{-# LANGUAGE ... #-}` pragma, except
`OverloadedStrings`, which must be enabled:

| Extension | Example |
|-----------|---------|
| LambdaCase | `\case { Just x -> x; Nothing -> 0 }` |
| MultiWayIf | `if \| x > 0 -> "pos" \| otherwise -> "non-pos"` |
| BangPatterns | `go !acc (x:xs) = go (acc + x) xs` |
| ScopedTypeVariables | `f :: forall a. ...` makes `a` usable in local signatures; pattern signatures `\(e :: SomeException) -> ...` |
| RecordWildCards | `f P{..} = name ++ show age` (in patterns) |
| NamedFieldPuns | `f P{name} = name` |
| BlockArguments | `forM_ xs \x -> print x` |
| DeriveFunctor, DeriveFoldable, DeriveTraversable | `data T a = ... deriving (Functor, Foldable)` |
| TupleSections | `map (, True) xs` |
| InstanceSigs | type signatures inside instances are accepted (and ignored) |
| NumericUnderscores | `1_000_000` |
| OverloadedStrings | string literals for `Text` (needs the pragma) |

### Not supported

User-defined pattern synonyms (the library uses a restricted form for
`Data.Sequence`), GADTs, type families, multi-parameter type classes, functional dependencies,
rank-N types (`forall` in argument positions), existential types, kind
signatures, view patterns, `GeneralizedNewtypeDeriving` (and deriving classes other than the
ones listed above), Template Haskell, `Typeable`/`Generic`, concurrency
(`forkIO`, `MVar`). Unsupported syntax produces a parse or type error.

## Standard library

The Prelude exports the same names as GHC's Prelude. Other modules:

| Module | Contents |
|--------|----------|
| `Data.List` | the usual list functions: `sort`, `sortBy`, `sortOn`, `nub`, `group`, `groupBy`, `partition`, `foldl'`, `intercalate`, `transpose`, `isPrefixOf`, `isInfixOf`, `tails`, `subsequences`, `permutations`, `unfoldr`, `zip4`, ... |
| `Data.List.Split` | `splitOn`, `splitWhen`, `wordsBy`, `linesBy`, `chunksOf`, `splitPlaces`, `endBy` (from the `split` package) |
| `Data.Char` | `ord`, `chr`, `isDigit`, `isAlpha`, `isSpace`, `toUpper`, `digitToInt`, ... |
| `Data.Maybe`, `Data.Either`, `Data.Function`, `Data.Ord`, `Data.Tuple`, `Data.Bool` | as in base (`fromMaybe`, `mapMaybe`, `on`, `(&)`, `comparing`, `Down`, `swap`, `bool`, ...) |
| `Data.Map`, `Data.Map.Strict` | balanced-tree maps (most of the containers API) |
| `Data.Set` | balanced-tree sets |
| `Data.Sequence` | sequences with fast access at both ends: `\|>`, `<\|`, `><`, `index`, `update`, `splitAt`, ...; the patterns `Empty`, `x :<\| rest`, `rest :\|> x` |
| `Data.IntMap`, `Data.IntMap.Strict`, `Data.IntSet` | Int-keyed versions (implemented with `Data.Map`/`Data.Set`) |
| `Data.Array` | immutable lazy arrays: `listArray`, `array`, `(!)`, `(//)`, `accumArray`, `bounds`, `elems`, `assocs`; `Data.Ix` |
| `Data.Array.ST`, `Data.Array.IO`, `Data.Array.MArray`, `Data.Array.Unboxed` | mutable arrays: `newArray`, `readArray`, `writeArray`, `modifyArray'`, `freeze`, `thaw`, `runSTUArray`, ... (`UArray` = `Array`) |
| `Data.IORef` | `newIORef`, `readIORef`, `writeIORef`, `modifyIORef'`, ... |
| `Control.Monad` | `forM_`, `forM`, `when`, `unless`, `replicateM`, `foldM`, `zipWithM_`, `filterM`, `forever`, `guard`, `mplus`, ... |
| `Control.Monad.State`, `Control.Monad.Reader`, `Control.Monad.Writer` | mtl-style monads and transformers: `State`/`StateT` (`get`, `put`, `modify`, `gets`, `evalState`, `execStateT`, ...), `Reader`/`ReaderT` (`ask`, `asks`, `local`), `Writer`/`WriterT` (`tell`, `runWriter`); `lift`, `liftIO` (also `Control.Monad.Trans.*`, `Control.Monad.IO.Class`) |
| `Control.Monad.ST`, `Data.STRef` | `runST`, `newSTRef`, `readSTRef`, `modifySTRef'`, ... |
| `Control.Applicative` | `Alternative` (`empty`, `<|>`, `many`, `some`), `liftA2`, `optional`, `ZipList` |
| `Control.Exception` | `catch`, `try`, `evaluate`, `throwIO`, `bracket`, `bracket_`, `bracketOnError`, `finally`, `onException`, `SomeException` |
| `Data.Foldable`, `Data.Traversable`, `Data.Functor`, `Data.Functor.Identity` | `for_`, `traverse_`, `foldrM`, `(<&>)`, `($>)`, `Identity`, ... |
| `Data.Monoid`, `Data.Semigroup` | `Sum`, `Product`, `Min`, `Max`, `First`, `Last`, `Any`, `All`, `Endo` |
| `Data.Bits` | `.&.`, `.\|.`, `xor`, `shiftL`, `shiftR`, `popCount`, `testBit`, ... |
| `Data.Ratio` | `Rational`, `%`, `numerator`, `denominator` |
| `Data.Int`, `Data.Word` | `Int64`, `Word8`, ... (as synonyms of `Int`, see below) |
| `Data.Text`, `Data.Text.IO` | a `Text` type with the common functions (`pack`, `unpack`, `strip`, `splitOn`, `words`, `replace`, ...) |
| `Data.ByteString`, `Data.ByteString.Char8`, `Data.ByteString.Lazy`, `Data.ByteString.Lazy.Char8` | byte strings: `pack`, `unpack`, `readFile`, `writeFile`, `getContents`, `lines`, `words`, `readInt`, `fromChunks`, ... (see below) |
| `Data.String` | `IsString` |
| `Text.Printf` | `printf` (to stdout or to a `String`), `hPrintf` |
| `Text.Read` | `readMaybe`, `readEither` |
| `Numeric` | `showHex`, `showOct`, `showFFloat`, `showEFloat`, `readHex`, ... |
| `System.IO` | handles (`stdin`, `stdout`, `stderr`), `hFlush`, `hSetBuffering`, `hGetLine`, `hPutStrLn`, `openFile`, `withFile`, `openTempFile`, `isEOF`, ... |
| `System.IO.Error` | `catchIOError`, `tryIOError`, `isDoesNotExistError`, `isPermissionError`, `isEOFError`, `isUserError`, `ioeGetFileName`, ... |
| `System.Directory` | `doesFileExist`, `doesDirectoryExist`, `removeFile`, `renameFile`, `copyFile`, `createDirectoryIfMissing`, `listDirectory`, `removeDirectoryRecursive`, `getCurrentDirectory`, `withCurrentDirectory`, ... |
| `System.Random` | `StdGen`, `mkStdGen`, `random`, `randomR`, `randoms`, `randomRs`, `split`, `getStdGen`, `newStdGen`, `randomRIO`, `randomIO` (from the `random` package) |
| `System.Exit`, `System.Environment`, `System.CPUTime`, `Debug.Trace` | `exitWith`, `exitFailure`, `getArgs`, `getProgName`, `getCPUTime`, `trace`, `traceShow`, ... |

## Differences from GHC

Most programs behave exactly as they do with GHC. These are the differences
you might notice:

- **`Int` doesn't overflow.** `Int` is an arbitrary-precision integer, like
  `Integer` (`maxBound :: Int` is still 2^63 - 1). `Int8`...`Int64` and
  `Word`...`Word64` are synonyms for `Int`, and `Float` is a synonym for
  `Double`.
- **Your own definitions win.** If your module defines a name that an import
  also provides (say you define your own `lookup`), husk uses your definition.
  GHC reports an "Ambiguous occurrence" error instead.
- **One exception type.** All exceptions are `SomeException` (and `IOError`
  is a synonym for it). `catch` and `try` handle every exception; you can't
  define your own exception types. `System.IO.Error` recognizes IO errors by
  their messages, so `catchIOError` and `isDoesNotExistError` work as usual.
- **No warnings, and some checks happen at run time.** Non-exhaustive
  patterns and missing instance methods are run-time errors (with a source
  location), not compile-time warnings.
- **Type signatures for pattern bindings** (`x, y :: Int` followed by
  `(x, y) = ...`) are checked only when they have no type variables or
  context; others (`n :: Num a => a`) are ignored. Type variables of
  instance heads don't scope over the instance's methods.
- **Only a simple kind check.** Using a type constructor without all its
  arguments where a complete type is needed (`f :: Maybe -> Int`) is reported,
  but other kind errors may show up as odd type errors, or not at all.
- **Mutable arrays** use one type for `IOArray`, `IOUArray`, `STArray` and
  `STUArray`, and the operations (`newArray`, `readArray`, ...) are overloaded
  over the monad with a husk-specific class (`MArrayMonad`) instead of GHC's
  multi-parameter `MArray` class. Code written for GHC works unchanged, unless
  it mentions `MArray` in a type signature.
- **Monad transformers without mtl classes.** `get`, `put`, `ask`, `tell`
  and friends work on the transformer at the top of the stack (`StateT s m`,
  ...). To use an inner layer, `lift` explicitly: in a
  `StateT s (Reader r)`, write `lift ask`. mtl's classes (`MonadState` and
  the rest) are multi-parameter classes, which husk doesn't have.
- **`runST` has the type `ST s a -> a`** (husk has no rank-2 types), so the
  type system doesn't stop an `STRef` escaping from `runST`.
- **`Text` is a wrapper around `String`**, so it is not faster than `String`.
- **`ByteString` is a wrapper around `String`** too, with one character
  (below 256) per byte, so it is not faster either. Strict and lazy
  ByteStrings are the same type (`fromStrict` and friends are identities).
  `readFile` and `writeFile` work byte for byte; the terminal and handles
  are text, converted with UTF-8.
- **`System.Random`** uses SplitMix, like the `random` package (so
  `mkStdGen 42` is the same generator), but the numbers drawn from a seed
  may differ from GHC's.
- **`Read`** works for numbers, characters, strings, lists, tuples, `Maybe`,
  `Either` and derived instances (including record syntax, as `show` prints it).
- **Defaulting** also picks `String` for `IsString` constraints. In the REPL,
  GHCi's extended rules apply: `()` for unconstrained show-able values, `IO`
  for monads (so `return 5` runs), lists for `Foldable`.
- **Memory limit.** A program that uses more than half of your physical
  memory (at most 8 GB) stops with a "heap exhausted" error, which is usually
  caused by infinite recursion. Set `HUSK_MAX_HEAP_MB` to change the limit.
- **Tabs** in source code count as moving to the next multiple of 8 columns,
  as in the Haskell report.
