import System.Random
import Data.List (sort, group)

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
  let (firstCoin, newGen) = random gen
      (secondCoin, newGen') = random newGen
      (thirdCoin, _) = random newGen'
  in (firstCoin, secondCoin, thirdCoin)

main :: IO ()
main = do
  -- the same seed as in the random package's documentation
  print (mkStdGen 42)
  print (fst (random (mkStdGen 100) :: (Int, StdGen)), threeCoins (mkStdGen 21))
  print (take 3 (randoms (mkStdGen 11) :: [Int]))
  print (take 3 (randoms (mkStdGen 11) :: [Double]))
  print (take 10 (randomRs ('a','z') (mkStdGen 3)), fst (randomR (1, 6 :: Int) (mkStdGen 359353)))
  print (fst (randomR ((1, 'a'), (3, 'c')) (mkStdGen 5) :: ((Int, Char), StdGen)))
  -- every outcome of a die about equally often
  let counts = map length (group (sort (take 6000 (randomRs (1, 6 :: Int) (mkStdGen 7)))))
  print (length counts, all (\c -> c > 900 && c < 1100) counts)
  print (all (\x -> x >= 10^30 && x <= 10^30 + 5) (take 50 (randomRs (10^30, 10^30 + 5 :: Integer) (mkStdGen 1))))
  print (all (\x -> x >= -3 && x <= 3) (take 50 (randomRs (3, -3 :: Int) (mkStdGen 9))))
  let (g1, g2) = split (mkStdGen 5)
  print (fst (next g1) /= fst (next g2))
  n <- randomRIO (1, 10 :: Int)
  d <- randomIO :: IO Double
  g <- newStdGen
  print (n >= 1 && n <= 10, d >= 0 && d <= 1, length (take 5 (randomRs (0, 1 :: Int) g)))
