-- Test.QuickCheck.  The random values change from run to run, so this
-- prints only results that don't depend on them: passing properties, and
-- the (unique) smallest counterexamples that shrinking finds.
import Test.QuickCheck
import Data.List (sort, nub)

prop_reverse :: [Int] -> Bool
prop_reverse xs = reverse (reverse xs) == xs

prop_sort :: [Int] -> Property
prop_sort xs = sort (sort xs) === sort xs

prop_insertLength :: Int -> [Int] -> Property
prop_insertLength x xs = not (null xs) ==> length (x : xs) == length xs + 1

prop_positive :: Positive Int -> NonNegative Integer -> Bool
prop_positive (Positive n) (NonNegative m) = n > 0 && m >= 0

prop_ordered :: OrderedList Int -> Bool
prop_ordered (Ordered xs) = sort xs == xs

prop_fun :: Fun Int Int -> [Int] -> Bool
prop_fun f xs = map (applyFun f) (reverse xs) == reverse (map (applyFun f) xs)

data Shape = Circle Int | Square Int deriving (Show, Eq)

instance Arbitrary Shape where
  arbitrary = oneof [Circle <$> choose (1, 10), Square <$> choose (1, 10)]

area :: Shape -> Int
area (Circle r) = 3 * r * r
area (Square s) = s * s

-- the last line of a failing test's output: the shrunk counterexample
counterexampleOf :: Testable p => p -> IO ()
counterexampleOf p = do
  r <- quickCheckWithResult stdArgs { chatty = False } p
  putStrLn (last (lines (output r)))

main :: IO ()
main = do
  quickCheck prop_reverse
  quickCheck prop_sort
  r0 <- quickCheckWithResult stdArgs { chatty = False } prop_insertLength
  print (isSuccess r0)
  quickCheck prop_positive
  quickCheck prop_ordered
  quickCheck prop_fun
  quickCheck (\s -> area s > 0)
  quickCheck (withMaxSuccess 500 (\x -> abs x >= (0 :: Int)))
  quickCheck (forAll (elements "abc") (`elem` "abc"))
  quickCheck (forAll (choose (1, 6 :: Int)) (\d -> d >= 1 && d <= 6))
  quickCheck (\x y -> x + y == y + (x :: Integer)) 
  quickCheck True
  quickCheck (counterexample "never shown" True)
  quickCheck (1 + 1 === (2 :: Int))
  quickCheck (2 + 2 === (5 :: Int))
  quickCheck (isSorted [3, 1, 2])
  -- failing properties shrink to the smallest counterexample
  counterexampleOf (\x -> x < (10 :: Int))
  counterexampleOf (\xs -> length (xs :: [Int]) < 3)
  counterexampleOf (\n -> n * n < (50 :: Int))
  counterexampleOf (\s -> 'z' `notElem` (s :: String))
  r <- quickCheckWithResult stdArgs { chatty = False } (\xs -> head (xs :: [Int]) >= 0)
  print (isSuccess r)
  xs <- sample' (vectorOf 3 (choose (0, 9 :: Int)))
  print (length xs, all ((== 3) . length) xs)
  where isSorted xs = property (and (zipWith (<=) xs (drop 1 xs)))
