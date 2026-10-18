{-# LANGUAGE NoImplicitPrelude #-}
-- exercises without the standard library (only pattern matching and recursion)
module Course.Recursion where

import Course.LimitedPrelude
import Course.Todo

sumsOf :: [Int] -> [Int]
sumsOf = go 0
  where go _ [] = []
        go acc (y : ys) = (acc + y) : go (acc + y) ys

merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys) = if x <= y then x : merge xs (y : ys) else y : merge (x : xs) ys

mymaximum :: (a -> a -> Bool) -> a -> [a] -> a
mymaximum _ initial [] = initial
mymaximum bigger initial (x : xs) = mymaximum bigger (if bigger x initial then x else initial) xs

-- not done yet
map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 = todo
