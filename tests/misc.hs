{-# LANGUAGE DeriveFunctor #-}
import qualified Data.Map as M
import Data.List (unfoldr, intercalate, transpose, group, sort, nub, partition, isPrefixOf, isInfixOf, tails, inits, subsequences, permutations)
import Data.Maybe (mapMaybe, fromMaybe, catMaybes)
import Data.Char (ord, chr, isAlpha)

data V2 = V2 Int Int deriving (Show, Eq, Ord, Read)

instance Num V2 where
  V2 a b + V2 c d = V2 (a + c) (b + d)
  V2 a b * V2 c d = V2 (a * c) (b * d)
  abs (V2 a b) = V2 (abs a) (abs b)
  signum (V2 a b) = V2 (signum a) (signum b)
  fromInteger n = V2 (fromInteger n) (fromInteger n)
  negate (V2 a b) = V2 (negate a) (negate b)

class Container f where
  empty :: f a
  insert :: a -> f a -> f a
  (+++) :: f a -> f a -> f a
  toL :: f a -> [a]

newtype Box a = Box [a] deriving Show

instance Container Box where
  empty = Box []
  insert x (Box xs) = Box (x : xs)
  Box a +++ Box b = Box (a ++ b)
  toL (Box xs) = xs

data Pair a = Pair a a deriving (Show, Functor)
data Rose a = Rose a [Rose a] deriving (Show, Functor)

instance Show (Box Int -> Int) where
  show _ = "<function on boxes>"

sign :: Int -> String
sign (-1) = "minus one"
sign 0 = "zero"
sign n = if n < 0 then "negative" else "positive"

caesar :: Int -> String -> String
caesar k = map shift
  where shift c | isAlpha c = chr ((ord c - ord 'a' + k) `mod` 26 + ord 'a')
                | otherwise = c

data Op = Plus | Minus deriving (Show, Read, Eq, Enum, Bounded)

main :: IO ()
main = do
  print (V2 1 2 + V2 10 20, V2 3 4 * 2, negate (V2 1 1), sum [V2 1 1, V2 2 2])
  print (read "V2 3 (-4)" :: V2, read "[Plus,Minus]" :: [Op], read " Just  5 " :: Maybe Int)
  print (toL (insert 1 (empty +++ Box [2, 3])) :: [Int])
  print (fmap (+ 1) (Pair 1 2), fmap show (Rose 1 [Rose 2 []]))
  print (show ((\(Box xs) -> length xs) :: Box Int -> Int))
  print (map sign [-1, 0, 5, -7])
  putStrLn (caesar 3 "hello, world")
  putStrLn "unicode: λ → ∀ ✓ äöü"
  print "λx"
  print (unfoldr (\n -> if n > 100 then Nothing else Just (n, n * 2)) 1)
  print (transpose ["abc", "de", "f"], map (\g -> (head g, length g)) (group (sort "mississippi")))
  print (nub [3, 1, 3, 2, 1 :: Int], partition even [1 .. 10 :: Int])
  print ("ab" `isPrefixOf` "abc", "ss" `isInfixOf` "mississippi", tails [1, 2 :: Int], inits "ab")
  print (length (subsequences [1 .. 10 :: Int]), take 4 (permutations [1, 2, 3 :: Int]))
  print (mapMaybe (\x -> if x > 2 then Just (x * x) else Nothing) [1 .. 5 :: Int], fromMaybe 0 Nothing, catMaybes [Just 'a', Nothing])
  let m = M.fromList [(c, ord c) | c <- "hello"]
  mapM_ (\(k, v) -> putStrLn (k : " -> " ++ show v)) (M.toList m)
  print (Just [1, 2] <> Nothing <> Just [3 :: Int], mconcat ["a", "b"], [LT ..])
  print (intercalate ", " (words "split these words"), zip3 [1 :: Int ..] "ab" [True, False])
  print (until (> 1000) (* 2) 1 :: Int, iterate (`div` 2) 100 !! 3 :: Int, gcd 12 18 :: Int, lcm 4 6 :: Int)
  print (showsPrec 11 (-1.5 :: Double) "", read "-2.5e-3" :: Double, fromIntegral (maxBound :: Int) + (1 :: Integer))
  print ([minBound .. maxBound :: Op], (toEnum 1 :: Op), [(x, y) | (x:y:_) <- ["ab", "c", "de"]])
