# The husk language

husk implements Haskell 2010 plus the extensions people use most in everyday
code. This page lists what is supported, which library modules exist, and
where husk behaves differently from GHC.

## Language features

### Supported

- **Modules**: `module M (exports) where`, `import M`, `import qualified M as N`,
  import lists, `hiding`, re-exporting with `module M` in export lists.
  Multi-module programs: modules are found next to the main file, then in
  the directories listed in `HUSK_PATH` (separated by colons). Literate
  Haskell (`.lhs` files, with `>` bird tracks or `\begin{code}` blocks).
  A file without a module header or `main` can be loaded into the REPL
  (`:l file.hs` or `husk -l file.hs`), as in GHCi.
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
  `Foldable`, `Traversable` and `Exception`; also standalone
  `deriving instance Show T`. A `newtype` can derive any class its
  representation type has (`newtype Score = Score Int deriving (Num)`), as
  with GeneralizedNewtypeDeriving. Contexts are inferred as in GHC,
  including ones like `Show (f (g a))`.
- **Type classes**: class declarations with superclasses, default methods,
  operator methods, and constructor classes (`class Container f where ...`).
  Instances can have contexts (`instance Show a => Show (Tree a)`) and be for
  partially applied types (`instance Functor (Either e)`,
  `instance Monad ((->) r)`). FlexibleInstances-style heads (`instance C (T Int)`)
  and contexts (`instance Show (f a) => Show (Wrap f a)`) work too.
- **Type inference**: full Hindley-Milner inference with type classes,
  polymorphic local `let`/`where` bindings, the monomorphism restriction, and
  Haskell's defaulting rules (`Integer`, then `Double`).
- **Laziness**: non-strict semantics everywhere: infinite lists, knot-tying
  (`fibs = 0 : 1 : zipWith (+) fibs (tail fibs)`), lazy arrays for dynamic
  programming, lazy `getContents`. `seq`, `$!`, `foldl'` and bang patterns
  force evaluation.
- **Numbers**: `Int` (64 bits, wrapping around on overflow as in GHC),
  `Integer` (arbitrary precision), `Int8`/`Int16`/`Int32`/`Int64`,
  `Word`/`Word8`/`Word16`/`Word32`/`Word64`, `Float` (single precision) and
  `Double`, `Rational` (`Data.Ratio`), with the standard numeric classes
  (`Num`, `Integral`, `Fractional`, `Floating`, `RealFrac`, `RealFloat`) and
  conversions (`fromIntegral`, `realToFrac`, `truncate`, `round`, ...).
- **Exceptions**: `throw`, `throwIO`, `catch`, `try`, `handle`, `bracket`
  and friends, with handlers chosen by type as in GHC: `SomeException`,
  `IOException`, `ErrorCall`, `ArithException`, and your own types
  (`data BankError = ... deriving Show` with `instance Exception BankError`).
  An uncaught `error` shows where it was called, like GHC's `CallStack`.

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
| GeneralizedNewtypeDeriving | `newtype Size = Size Int deriving (Eq, Ord, Num)` |
| DeriveAnyClass | for `Exception` only: `data Oops = Oops deriving (Show, Exception)` |
| FlexibleInstances, FlexibleContexts, TypeSynonymInstances | `instance Buffer (JoinList (Score, Size) String)` |
| NoImplicitPrelude | `import MyPrelude` instead of the Prelude |
| NumericUnderscores | `1_000_000` |
| OverloadedStrings | string literals for `Text` (needs the pragma) |

### Not supported

These are left out on purpose: each would make husk considerably bigger and
more complex, and introductory material rarely needs them.

- multi-parameter type classes and functional dependencies (so mtl's
  `MonadState s m` style, see below), GADTs, type families
- rank-N types (`forall` in argument positions) and existential types
  (including the pattern `\(SomeException e) -> ...`; write
  `\e -> ... (e :: SomeException)` instead)
- user-defined pattern synonyms (the library uses a restricted form for
  `Data.Sequence`), view patterns, kind signatures
- Template Haskell, `Typeable`/`Generic`, the FFI, concurrency (`forkIO`,
  `MVar`, STM)

Using them gives an error that says the feature isn't supported.

## Standard library

The Prelude exports the same names as GHC's Prelude. Other modules:

| Module | Contents |
|--------|----------|
| `Data.List` | the usual list functions: `sort`, `sortBy`, `sortOn`, `nub`, `group`, `groupBy`, `partition`, `foldl'`, `intercalate`, `transpose`, `isPrefixOf`, `isInfixOf`, `tails`, `subsequences`, `permutations`, `unfoldr`, `zip4`, ... |
| `Data.List.Split` | `splitOn`, `splitWhen`, `wordsBy`, `linesBy`, `chunksOf`, `splitPlaces`, `endBy` (from the `split` package) |
| `Data.Char` | `ord`, `chr`, `isDigit`, `isAlpha`, `isSpace`, `toUpper`, `digitToInt`, ... |
| `Data.Maybe`, `Data.Either`, `Data.Function`, `Data.Ord`, `Data.Tuple`, `Data.Bool` | as in base (`fromMaybe`, `mapMaybe`, `on`, `(&)`, `comparing`, `Down`, `swap`, `bool`, ...) |
| `Data.List.NonEmpty` | non-empty lists `x :\| xs`: `toList`, `fromList`, `nonEmpty`, `head`, `map`, `groupBy`, ... |
| `Data.Map`, `Data.Map.Strict` | balanced-tree maps (most of the containers API) |
| `Data.Set` | balanced-tree sets |
| `Data.Sequence` | sequences with fast access at both ends: `\|>`, `<\|`, `><`, `index`, `update`, `splitAt`, ...; the patterns `Empty`, `x :<\| rest`, `rest :\|> x` |
| `Data.IntMap`, `Data.IntMap.Strict`, `Data.IntSet` | Int-keyed versions (implemented with `Data.Map`/`Data.Set`) |
| `Data.Tree` | rose trees: `Node`, `rootLabel`, `subForest`, `flatten`, `levels`, `foldTree`, `unfoldTree`, `drawTree` |
| `Data.Array` | immutable lazy arrays: `listArray`, `array`, `(!)`, `(//)`, `accumArray`, `bounds`, `elems`, `assocs`; `Data.Ix` |
| `Data.Array.ST`, `Data.Array.IO`, `Data.Array.MArray`, `Data.Array.Unboxed` | mutable arrays: `newArray`, `readArray`, `writeArray`, `modifyArray'`, `freeze`, `thaw`, `runSTUArray`, ... (`UArray` = `Array`) |
| `Data.IORef` | `newIORef`, `readIORef`, `writeIORef`, `modifyIORef'`, ... |
| `Control.Monad` | `forM_`, `forM`, `when`, `unless`, `replicateM`, `foldM`, `zipWithM_`, `filterM`, `forever`, `guard`, `mplus`, ... |
| `Control.Monad.State`, `Control.Monad.Reader`, `Control.Monad.Writer` | mtl-style monads and transformers: `State`/`StateT` (`get`, `put`, `modify`, `gets`, `evalState`, `execStateT`, ...), `Reader`/`ReaderT` (`ask`, `asks`, `local`), `Writer`/`WriterT` (`tell`, `runWriter`); `lift`, `liftIO` (also `Control.Monad.Trans.*`, `Control.Monad.IO.Class`) |
| `Control.Monad.ST`, `Data.STRef` | `runST`, `newSTRef`, `readSTRef`, `modifySTRef'`, ... |
| `Control.Applicative` | `Alternative` (`empty`, `<|>`, `many`, `some`), `liftA2`, `optional`, `ZipList` |
| `Control.Exception` | `Exception` (`toException`, `fromException`, `displayException`), `SomeException`, `IOException`, `ErrorCall`, `ArithException`; `catch`, `catchJust`, `handle`, `try`, `tryJust`, `evaluate`, `throw`, `throwIO`, `bracket`, `bracket_`, `bracketOnError`, `finally`, `onException` |
| `Control.Arrow` | `first`, `second`, `***`, `&&&`, `>>>`, `<<<`, `left`, `right`, `+++`, `\|\|\|` (for functions) |
| `Data.Foldable`, `Data.Traversable`, `Data.Functor`, `Data.Functor.Identity` | `for_`, `traverse_`, `foldrM`, `(<&>)`, `($>)`, `Identity`, ... |
| `Data.Monoid`, `Data.Semigroup` | `Sum`, `Product`, `Min`, `Max`, `First`, `Last`, `Any`, `All`, `Endo` |
| `Data.Bits` | `.&.`, `.\|.`, `xor`, `shiftL`, `shiftR`, `popCount`, `testBit`, ... |
| `Data.Ratio` | `Rational`, `%`, `numerator`, `denominator` |
| `Data.Int`, `Data.Word` | `Int8` ... `Int64`, `Word` ... `Word64` |
| `Data.Text`, `Data.Text.IO`, `Data.Text.Lazy` | a `Text` type with the common functions (`pack`, `unpack`, `strip`, `splitOn`, `words`, `replace`, ...) |
| `Data.Text.Encoding` | `encodeUtf8`, `decodeUtf8`, `decodeUtf8'` (which reports invalid UTF-8), `decodeLatin1` |
| `Data.ByteString`, `Data.ByteString.Char8`, `Data.ByteString.Lazy`, `Data.ByteString.Lazy.Char8` | byte strings: `pack`, `unpack`, `readFile`, `writeFile`, `getContents`, `lines`, `words`, `readInt`, `fromChunks`, ... (see below) |
| `Data.String` | `IsString` |
| `Text.Printf` | `printf` (to stdout or to a `String`), `hPrintf` |
| `Text.Read` | `readMaybe`, `readEither` |
| `Numeric` | `showHex`, `showOct`, `showFFloat`, `showEFloat`, `readHex`, ... |
| `System.IO` | handles (`stdin`, `stdout`, `stderr`), `hFlush`, `hSetBuffering`, `hGetLine`, `hPutStrLn`, `openFile`, `withFile`, `openTempFile`, `isEOF`, ... |
| `System.IO.Error` | `catchIOError`, `tryIOError`, `isDoesNotExistError`, `isPermissionError`, `isEOFError`, `isUserError`, `ioeGetFileName`, ... |
| `System.Directory` | `doesFileExist`, `doesDirectoryExist`, `removeFile`, `renameFile`, `copyFile`, `createDirectoryIfMissing`, `listDirectory`, `removeDirectoryRecursive`, `getCurrentDirectory`, `withCurrentDirectory`, ... |
| `System.Random` | `StdGen`, `mkStdGen`, `random`, `randomR`, `randoms`, `randomRs`, `split`, `getStdGen`, `newStdGen`, `randomRIO`, `randomIO` (from the `random` package) |
| `Control.Monad.Random` | `Rand`, `RandT`, `getRandom`, `getRandomR`, `getRandoms`, `evalRand`, `runRand`, `evalRandIO`, `fromList`, `uniform` (from the `MonadRandom` package) |
| `Test.QuickCheck` | `quickCheck`, `quickCheckWith`, `verboseCheck`, `Arbitrary`, `Gen`, `forAll`, `==>`, `===`, `.&&.`, `counterexample`, `label`, `classify`, `choose`, `elements`, `oneof`, `frequency`, `sized`, `listOf`, `vectorOf`, `suchThat`, `Positive`, `NonNegative`, `NonEmptyList`, `OrderedList`, `Fun`, ... (a subset of the `QuickCheck` package, with shrinking) |
| `Codec.Picture` | `generateImage`, `pixelAt`, `PixelRGB8`, `PixelRGBA8`, `Pixel8`, `writePng`, `encodePng` (from the `JuicyPixels` package; images can be written but not read) |
| `System.Exit`, `System.Environment`, `System.CPUTime`, `System.IO.Unsafe`, `Debug.Trace` | `exitWith`, `exitFailure`, `getArgs`, `getProgName`, `getCPUTime`, `unsafePerformIO`, `trace`, `traceShow`, ... |

husk can't use packages from Hackage; it includes its own versions of the
packages above instead. See [LEARNING.md](LEARNING.md#libraries) for why,
and for using your own modules with `HUSK_PATH`.

## Differences from GHC

Most programs behave exactly as they do with GHC. These are the differences
you might notice:

- **Your own definitions hide the Prelude's.** If your module defines a name
  that the Prelude also provides (say you define your own `lookup` or
  `filter`, as exercises often ask), husk uses your definition, where GHC
  reports an "Ambiguous occurrence" error. A clash with a module you import
  explicitly is an error, as in GHC.
- **Exceptions**: `exitWith` and `exitFailure` stop the program even inside
  `catch` (in GHC, a handler for `SomeException` would catch them).
  `Int64` is the same type as `Int`.
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
  the rest) are multi-parameter classes, which husk doesn't have, so
  signatures like `MonadState Int m => m ()` and `deriving (MonadState s)`
  don't work.
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
  may differ from GHC's. For the same reason `Test.QuickCheck` tries
  different random values than GHC's QuickCheck, so a failing property may
  be reported with a different (equally valid) counterexample.
- **`Codec.Picture`** writes PNG files without compression, so they are
  bigger than JuicyPixels' (the pixels are the same).
- **The REPL keeps the Prelude in scope** after loading a module with
  `NoImplicitPrelude`.
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
