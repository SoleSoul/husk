import Data.Array.ST
import Data.Array.IO
import Data.Array.Unboxed
import Control.Monad
import Data.STRef

sieve :: Int -> UArray Int Bool
sieve n = runSTUArray $ do
  arr <- newArray (2, n) True
  forM_ [2 .. n] $ \i -> do
    isP <- readArray arr i
    when (isP && i * i <= n) $
      forM_ [i * i, i * i + i .. n] $ \j -> writeArray arr j False
  return arr

-- in-place prefix sums with an STArray
prefixSums :: [Int] -> [Int]
prefixSums xs = elems $ runSTArray $ do
  let n = length xs
  arr <- newListArray (1, n) xs
  forM_ [2 .. n] $ \i -> do
    prev <- readArray arr (i - 1)
    modifyArray' arr i (+ prev)
  return arr

main :: IO ()
main = do
  let s = sieve 100
  print [i | (i, True) <- assocs s]
  print (length (filter id (elems (sieve 1000000))))
  print (prefixSums [1 .. 10])
  arr <- newArray ((0, 0), (2, 2)) 0 :: IO (IOArray (Int, Int) Int)
  forM_ [0 .. 2] $ \i -> writeArray arr (i, i) (i + 1)
  getElems arr >>= print
  lazyArr <- newArray_ (1, 3) :: IO (IOArray Int String)
  writeArray lazyArr 2 "only this one"
  readArray lazyArr 2 >>= putStrLn
  frozen <- freeze arr
  print (frozen ! (1, 1), bounds frozen)
