-- Data.List.Split: the most used functions of the `split` package.
module Data.List.Split (
  splitOn, splitOneOf, splitWhen, wordsBy, linesBy, chunksOf, splitPlaces, splitEvery, endBy
  ) where

import Data.List (isPrefixOf)

-- | splitOn "," "a,b,,c" == ["a","b","","c"]
splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn [] _ = error "splitOn: empty delimiter"
splitOn delim xs = go xs
  where
    n = length delim
    go s = case breakOn s of
      (chunk, Nothing) -> [chunk]
      (chunk, Just rest) -> chunk : go rest
    breakOn [] = ([], Nothing)
    breakOn s@(c:cs)
      | delim `isPrefixOf` s = ([], Just (drop n s))
      | otherwise = let (chunk, rest) = breakOn cs in (c : chunk, rest)

-- | splitOneOf ";," "a;b,c" == ["a","b","c"]
splitOneOf :: Eq a => [a] -> [a] -> [[a]]
splitOneOf delims = splitWhen (`elem` delims)

-- | splitWhen (<0) [1,3,-4,5,7,-9,0,2] == [[1,3],[5,7],[0,2]]
splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen p xs = case break p xs of
  (chunk, []) -> [chunk]
  (chunk, _ : rest) -> chunk : splitWhen p rest

-- | like splitWhen, but drops empty pieces
wordsBy :: (a -> Bool) -> [a] -> [[a]]
wordsBy p xs = filter (not . null) (splitWhen p xs)

-- | like splitWhen, but a final empty piece is dropped
linesBy :: (a -> Bool) -> [a] -> [[a]]
linesBy p xs = case splitWhen p xs of
  pieces | not (null pieces) && null (last pieces) -> init pieces
         | otherwise -> pieces

-- | endBy ";" "a;b;" == ["a","b"]
endBy :: Eq a => [a] -> [a] -> [[a]]
endBy delim xs = case splitOn delim xs of
  pieces | not (null pieces) && null (last pieces) -> init pieces
         | otherwise -> pieces

-- | chunksOf 3 [1..7] == [[1,2,3],[4,5,6],[7]]
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

splitEvery :: Int -> [a] -> [[a]]
splitEvery = chunksOf

-- | splitPlaces [2,3] [1..6] == [[1,2],[3,4,5]]
splitPlaces :: [Int] -> [a] -> [[a]]
splitPlaces [] _ = []
splitPlaces _ [] = []
splitPlaces (n:ns) xs = take n xs : splitPlaces ns (drop n xs)
