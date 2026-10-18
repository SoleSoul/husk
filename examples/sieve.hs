-- Sieve of Eratosthenes with an accumulating array
import Data.Array

primesUpTo :: Int -> [Int]
primesUpTo n = [i | (i, True) <- assocs sieve]
  where
    sieve = accumArray (\_ b -> b) True (2, n)
              [(m, False) | i <- [2 .. isqrt n], m <- [i * i, i * i + i .. n]]
    isqrt = floor . sqrt . fromIntegral

main :: IO ()
main = do
  let ps = primesUpTo 1000000
  print (length ps)
  print (last ps)
  print (sum (take 1000 ps))
