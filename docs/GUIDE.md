# A practical guide to Haskell with husk

Haskell often offers several ways to do the same thing. This guide picks one
good way for each common task, so you can get on with solving problems. All
of it works in husk, and since husk implements standard Haskell, all of it
works with GHC too.

## Program shape

```haskell
module Main where            -- optional for the main module

import qualified Data.Map.Strict as Map
import Data.List (sortOn, foldl')

-- pure functions first: small, with type signatures
solve :: [Int] -> Int
solve = sum . filter even

-- IO only at the edges
main :: IO ()
main = do
  input <- getContents
  let nums = map read (lines input) :: [Int]
  print (solve nums)
```

- Write a type signature for every top-level function. It documents the code
  and gives much better error messages.
- Keep IO in `main` (and a few helpers). Parse the input into data, solve with
  pure functions, print the result.
- Run with `husk prog.hs < input.txt`, or explore in the REPL with
  `:l prog.hs`, then call `solve` on small inputs.

## Reading input

| You have | Write |
|----------|-------|
| one number per line | `map read . lines` |
| numbers separated by spaces | `map read . words` |
| comma-separated values | `map read . splitOn ","` (from `Data.List.Split`) |
| a grid of characters | `lines input`, then index with `!!`, or build an array or map (below) |
| blocks separated by blank lines | `splitOn "\n\n" input` |
| a number that may be malformed | `readMaybe s :: Maybe Int` (from `Text.Read`) |

Read the whole input at once with `getContents`, which is lazy, so it also
works for big inputs. Use `getLine` only for interactive programs.

## Choosing a data structure

| Need | Use |
|------|-----|
| a sequence you traverse from the front | a list `[a]` |
| lookup by key | `Data.Map.Strict` (import qualified as `Map`) |
| a set of things seen | `Data.Set` (as `Set`) |
| a queue (e.g. for breadth-first search) | `Data.Sequence` (as `Seq`): add with `\|>`, take with the pattern `x :<\| rest` |
| a counter or histogram | `Map.fromListWith (+) [(x, 1) \| x <- xs]` |
| fixed-size indexed data, grids | `Data.Array` (`listArray`, `!`) |
| a grid with neighbours | `Map.Map (Int, Int) Char`, built with `Map.fromList [((r, c), ch) \| (r, row) <- zip [0..] rows, (c, ch) <- zip [0..] row]` |
| updating a big array in place | `Data.Array.ST` with `runSTUArray` |
| a record of named fields | `data Config = Config { size :: Int, name :: String }` |
| "a value or nothing" | `Maybe a` |
| "a value or an error message" | `Either String a` |

Prefer `Data.Map.Strict` over `Data.Map`: values are evaluated as they're
inserted, which avoids building up unevaluated work.

## Loops

Haskell has no loops. Instead, pick the shape of the computation:

| Shape | Use |
|-------|-----|
| transform every element | `map f xs`, or a list comprehension `[f x \| x <- xs, p x]` |
| keep some elements | `filter p xs` |
| combine all elements into one value | `sum`, `product`, `maximum`, or `foldl' step start xs` |
| a running state over a list | `foldl'` with a tuple or record as the accumulator |
| repeat until done | a recursive helper: `go state = if done state then state else go (step state)` |
| all intermediate states | `iterate step start`, then `takeWhile`/`!!` |
| side effects for each element | `forM_ xs $ \x -> do ...` |
| collect results of effects | `results <- forM xs $ \x -> do ...` |

Use `foldl'` (from `Data.List`), not `foldl` or `foldr`, when combining a big
list into a number or a map. For a hand-written loop, put the accumulator in
a bang pattern: `go !acc (x:xs) = go (acc + x) xs`.

## Defining your own types

```haskell
data Shape = Circle Double | Rect Double Double
  deriving (Show, Eq, Ord)

area :: Shape -> Double
area (Circle r) = pi * r * r
area (Rect w h) = w * h
```

- Add `deriving (Show, Eq, Ord)` to almost every type: you can print values,
  compare them, use them as `Map` keys, and sort them.
- Use `newtype` for a type with exactly one field (`newtype Id = Id Int`).
- Write your own type classes rarely. Most code needs only data types and
  functions.

## Error handling

- Use `Maybe` / `Either String` for expected failures, and chain them with
  `do` notation.
- Use `error "message"` for impossible cases (it stops the program with that
  message).
- `head`, `tail`, `!!`, `fromJust` and `Map.!` fail on bad input. Prefer
  pattern matching, `Map.lookup` or `Map.findWithDefault`.

## Strings and output

- `show` turns a value into a `String`; `putStrLn` prints one; `print x` is
  `putStrLn (show x)`.
- `unwords`, `unlines` and `intercalate ", "` join strings.
- For formatted numbers, use `printf "%.2f\n" x` (from `Text.Printf`).

## Things you can skip at first

Haskell has a lot of advanced machinery. You can write almost any program
without:

- monad transformers: pass state explicitly or use `Control.Monad.State`;
- GADTs, type families, lenses, `Data.Typeable`, Template Haskell (husk
  doesn't support these anyway);
- `Data.Text` and `ByteString`, since `String` is fine for problem solving;
- writing `Functor`/`Monad` instances, except as an exercise.

## husk-specific tips

- `husk --check prog.hs` typechecks without running.
- `:t expr` in the REPL shows any expression's type, `:i name` shows a
  function, type or class, and `:browse Data.Map` lists what a module offers.
- `Debug.Trace.trace "msg" value` prints while evaluating, which is handy for
  debugging pure code.
- husk's `Int` never overflows (it's arbitrary precision), so a program that
  relies on overflow behaves differently than with GHC.
