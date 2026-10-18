{-# LANGUAGE LambdaCase #-}
module Main (main) where

import Data.Char (isUpper)
import qualified Data.List as L

infixl 6 |+|
(|+|) :: (Int, Int) -> (Int, Int) -> (Int, Int)
(a, b) |+| (c, d) = (a + c, b + d)

infixr 5 :+:
data Expr = Lit Int | Expr :+: Expr | Neg Expr deriving (Show, Eq)

eval :: Expr -> Int
eval = \case
  Lit n -> n
  a :+: b -> eval a + eval b
  Neg e -> negate (eval e)

data V = V { vx, vy :: Double } deriving (Show, Eq, Ord)

norm :: V -> Double
norm V { vx = x, vy = y } = sqrt (x * x + y * y)

classify :: Int -> String
classify n
  | n < 0 = "negative"
  | n == 0 = "zero"
  | even n, n > 100 = "big even"
  | Just w <- lookup n table = w
  | otherwise = let s = show n in "number " ++ s
  where table = [(1, "one"), (2, "two")]

bmi :: Double -> Double -> String
bmi w h
  | v <= under = "under"
  | v <= normal = "normal"
  | otherwise = "over"
  where v = w / h ^ 2
        (under, normal) = (18.5, 25.0)

collatzLen :: Int -> Int
collatzLen = go 1
  where
    go acc 1 = acc
    go acc n
      | even n = go (acc + 1) (n `div` 2)
      | otherwise = go (acc + 1) (3 * n + 1)

firstTwo :: [a] -> Maybe (a, a)
firstTwo (x:y:_) = Just (x, y)
firstTwo _ = Nothing

capitals :: String -> String
capitals s@(c:_) | isUpper c = "starts upper: " ++ s
capitals s = "other: " ++ s

lazyMatch :: (Int, Int) -> Int
lazyMatch ~(a, _) = 1

strictSum :: [Int] -> Int
strictSum = go 0
  where go !acc [] = acc
        go !acc (x:xs) = go (acc + x) xs

applyTwice :: (a -> a) -> a -> a
applyTwice f = f . f

compose3 :: [Int] -> Int
compose3 = sum . map (* 2) . filter odd

data Animal = Dog | Cat | Bird deriving (Show, Eq, Ord, Enum, Bounded)

sound :: Animal -> String
sound a = case a of
  Dog -> "woof"
  Cat -> "meow"
  _ -> "tweet"

multiIf :: Int -> String
multiIf x = if | x > 10 -> "large" | x > 5 -> "medium" | otherwise -> "small"

main :: IO ()
main = do
  print ((1, 2) |+| (3, 4) |+| (10, 20))
  print (Lit 1 :+: Lit 2 :+: Neg (Lit 4), eval (Lit 1 :+: Lit 2 :+: Neg (Lit 4)))
  print (V 3 4, norm (V 3 4), V { vy = 1, vx = 2 })
  mapM_ (putStrLn . classify) [-5, 0, 1, 2, 7, 200]
  putStrLn (bmi 70 1.8)
  print (collatzLen 27)
  print (firstTwo "abc", firstTwo [1 :: Int])
  putStrLn (capitals "Hello") >> putStrLn (capitals "world")
  print (lazyMatch undefined, strictSum [1 .. 100])
  print (applyTwice (* 3) 7, applyTwice ('x' :) "y", compose3 [1 .. 10])
  print (map sound [minBound .. maxBound])
  print (map multiIf [1, 7, 20])
  print (map (subtract 1) [1, 2, 3], map (`div` 2) [10, 11], map (2 ^) [1, 2, 3 :: Int])
  print ((`elem` "aeiou") 'e', (10 -) 3, (/ 2) 9)
  print [if even x then x else -x | x <- [1 .. 6 :: Int]]
  print [(i, j) | i <- [1 .. 4 :: Int], let j = i * i, j > 4]
  print [x | Just x <- [Just 1, Nothing, Just 3 :: Maybe Int]]
  print [1, 3 .. 11 :: Int]
  print [10, 8 .. 1 :: Int]
  print ['a' .. 'f']
  print [1.0, 1.5 .. 3.0 :: Double]
  print (let (a, b) = (3, 4); c = a * b in c + 1 :: Int)
  let go 0 = []
      go n = n : go (n - 1)
  print (go (5 :: Int))
  print $ L.sortOn negate [3, 1, 2 :: Int]
  print $ zipWith ($) [(+ 1), (* 2)] [10, 20 :: Int]
  print $ foldr (\x acc -> x : take 2 acc) [] [1 .. 10 :: Int]
  print $ (\(a, b) c -> a + b + c) (1, 2) (3 :: Int)
  print $ let xs@(x:rest) = [5, 6, 7 :: Int] in (xs, x, rest)
  print $ case [1, 2, 3 :: Int] of { [] -> 0; [_] -> 1; _ -> 99 }
  print $ maybe "none" show (Just (42 :: Int))
  print $ either length negate (Left "abc" :: Either String Int)
  print $ uncurry (+) (3, 4 :: Int)
  print $ (,) 1 'x'
  print $ (,,) 1 'x' "y"
  print (- 5 + 3 :: Int, negate (-3) :: Int, abs (-7) :: Int, 7 - (-2) :: Int)
  print (show (Neg (Lit (-3))))
  print (fromEnum 'A', toEnum 98 :: Char, succ 'a', pred 10 :: Int)
  print (minBound :: Int, maxBound :: Char)
  where
    unused = ()
