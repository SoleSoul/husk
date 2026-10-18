-- GHC.Exts: only the list functions sortWith, groupWith and the.
module GHC.Exts (sortWith, groupWith, the) where
import Husk.Base
import Data.List (sortBy, groupBy)
import Data.Ord (comparing)

sortWith :: Ord b => (a -> b) -> [a] -> [a]
sortWith f = sortBy (comparing f)

groupWith :: Ord b => (a -> b) -> [a] -> [[a]]
groupWith f = groupBy (\a b -> f a == f b) . sortWith f

-- the one value that all elements of a non-empty list are equal to
the :: Eq a => [a] -> a
the (x : xs)
  | all (== x) xs = x
  | otherwise = errorWithoutStackTrace "GHC.Exts.the: non-identical list"
the [] = errorWithoutStackTrace "GHC.Exts.the: empty list"
