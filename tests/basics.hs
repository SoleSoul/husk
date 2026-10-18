module Main where

import Data.List (sortBy, foldl')
import Data.Char (toUpper, isDigit)

data Shape = Circle Double | Rect Double Double
  deriving (Show, Eq)

area :: Shape -> Double
area (Circle r) = pi * r * r
area (Rect w h) = w * h

data Color = Red | Green | Blue deriving (Show, Eq, Ord, Enum, Bounded)

data Person = Person { name :: String, age :: Int } deriving (Show, Eq)

data Tree a = Leaf | Node (Tree a) a (Tree a)

insert :: Ord a => a -> Tree a -> Tree a
insert x Leaf = Node Leaf x Leaf
insert x t@(Node l v r)
  | x < v = Node (insert x l) v r
  | x > v = Node l v (insert x r)
  | otherwise = t

toList :: Tree a -> [a]
toList Leaf = []
toList (Node l v r) = toList l ++ [v] ++ toList r

instance Show a => Show (Tree a) where
  show t = "Tree " ++ show (toList t)

class Describable a where
  describe :: a -> String
  describe _ = "something"
  name' :: a -> String

instance Describable Color where
  name' c = "color " ++ show c

instance Describable Bool where
  describe b = if b then "yes" else "no"
  name' _ = "bool"

fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n - 1)

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

primes :: [Int]
primes = sieve [2..] where sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

main :: IO ()
main = do
  print (map area [Circle 1, Rect 2 3])
  print [minBound .. maxBound :: Color]
  print (succ Red, fromEnum Blue, [Red ..])
  let p = Person { name = "Ann", age = 30 }
  print p
  print (p { age = 31 })
  putStrLn (name p)
  print (toList (foldr insert Leaf [5, 3, 8, 1, 4, 7, 9 :: Int]))
  print (foldr insert Leaf "hello")
  putStrLn (describe Red ++ ", " ++ name' Green ++ ", " ++ describe True)
  print (fact 25)
  print (take 15 fibs)
  print (takeWhile (< 40) primes)
  print (foldl' (+) 0 [1..100000 :: Int])
  print (sortBy (\a b -> compare b a) [3,1,4,1,5,9,2,6 :: Int])
  print (map toUpper "shout", filter isDigit "a1b2c3")
  print (words "  the quick  brown fox ", unwords ["a", "b"], lines "x\ny\n")
  print (zip [1 :: Int ..] "abc")
  print [(x, y) | x <- [1..3 :: Int], y <- "ab", odd x]
  print (show 3.5, show (-2 :: Int), Just (-1.5 :: Double), [Left 1, Right "x" :: Either Int String])
  print (div 7 2 :: Int, mod (-7) 2 :: Int, quot (-7) 2 :: Int, rem (-7) 2 :: Int, divMod 7 (-2) :: (Int, Int))
  print (2 ^ 64 :: Integer, 7 / 2 :: Double, fromIntegral (3 :: Int) * 1.5 :: Double)
  print (truncate (3.7 :: Double) :: Int, round (2.5 :: Double) :: Int, round (3.5 :: Double) :: Int, ceiling (2.1 :: Double) :: Int, floor (-2.1 :: Double) :: Int)
  print (sqrt 2 :: Double, 1 / 3 :: Double, 1.0e-2 :: Double, 12345678.9 :: Double, 0.1 + 0.2 :: Double)
  print (read "42" :: Int, read "[1,2,3]" :: [Int], read " ( 3 , \"hi\" ) " :: (Int, String))
  print (compare 1 2, max "abc" "abd", minimum [3,1,2 :: Int], maximum "hello")
  print (lookup 2 [(1, "one"), (2, "two")], elem 3 [1,2,3 :: Int], null [], length "four")
  print (case (1 :: Int, "x") of (n, s) | n > 0 -> s ++ "!" ; _ -> "?")
  mapM_ print [Just 'a', Nothing]
  print $ let (a, b) = span even [2,4,6,7,8 :: Int] in (a, b)
  print (showsPrec 11 (-5 :: Int) "", show (Just (Just Nothing :: Maybe (Maybe Int))))
  print (concatMap show [1,2,3 :: Int], replicate 3 'x', reverse [1,2,3 :: Int])
  print ('a', "tab\there", '\n', "quote\"d", "\1234\&5")
