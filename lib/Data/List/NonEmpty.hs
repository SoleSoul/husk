-- Data.List.NonEmpty: lists with at least one element, written  x :| xs.
-- Usually imported qualified:  import qualified Data.List.NonEmpty as NE
module Data.List.NonEmpty (
  NonEmpty(..), toList, fromList, nonEmpty, head, tail, last, init, length, map,
  (<|), cons, uncons, reverse, sort, sortBy, sortOn, sortWith, nub, zip, zipWith,
  filter, partition, takeWhile, dropWhile, take, drop, group, groupBy, group1, groupWith,
  groupAllWith, iterate, repeat, cycle, insert, scanl, scanr, xor, (!!), singleton, appendList
  ) where

import Prelude hiding (head, tail, last, init, length, map, reverse, zip, zipWith, filter,
                       takeWhile, dropWhile, take, drop, iterate, repeat, cycle, scanl, scanr, (!!))
import qualified Prelude as P
import qualified Data.List as L
import Data.Ord (comparing)
import Husk.Base (NonEmpty(..))

infixr 5 <|

toList :: NonEmpty a -> [a]
toList (x :| xs) = x : xs

fromList :: [a] -> NonEmpty a
fromList (x : xs) = x :| xs
fromList [] = error "NonEmpty.fromList: empty list"

nonEmpty :: [a] -> Maybe (NonEmpty a)
nonEmpty [] = Nothing
nonEmpty (x : xs) = Just (x :| xs)

singleton :: a -> NonEmpty a
singleton x = x :| []

head :: NonEmpty a -> a
head (x :| _) = x

tail :: NonEmpty a -> [a]
tail (_ :| xs) = xs

last :: NonEmpty a -> a
last (x :| xs) = P.last (x : xs)

init :: NonEmpty a -> [a]
init (x :| xs) = P.init (x : xs)

length :: NonEmpty a -> Int
length (_ :| xs) = 1 + P.length xs

map :: (a -> b) -> NonEmpty a -> NonEmpty b
map f (x :| xs) = f x :| P.map f xs

(<|), cons :: a -> NonEmpty a -> NonEmpty a
y <| (x :| xs) = y :| (x : xs)
cons = (<|)

uncons :: NonEmpty a -> (a, Maybe (NonEmpty a))
uncons (x :| xs) = (x, nonEmpty xs)

appendList :: NonEmpty a -> [a] -> NonEmpty a
appendList (x :| xs) ys = x :| (xs ++ ys)

lift :: ([a] -> [b]) -> NonEmpty a -> NonEmpty b
lift f = fromList . f . toList

reverse :: NonEmpty a -> NonEmpty a
reverse = lift P.reverse

sort :: Ord a => NonEmpty a -> NonEmpty a
sort = lift L.sort

sortBy :: (a -> a -> Ordering) -> NonEmpty a -> NonEmpty a
sortBy f = lift (L.sortBy f)

sortOn :: Ord b => (a -> b) -> NonEmpty a -> NonEmpty a
sortOn f = lift (L.sortOn f)

sortWith :: Ord b => (a -> b) -> NonEmpty a -> NonEmpty a
sortWith f = lift (L.sortBy (comparing f))

nub :: Eq a => NonEmpty a -> NonEmpty a
nub = lift L.nub

insert :: Ord a => a -> [a] -> NonEmpty a
insert x xs = fromList (L.insert x xs)

zip :: NonEmpty a -> NonEmpty b -> NonEmpty (a, b)
zip (x :| xs) (y :| ys) = (x, y) :| P.zip xs ys

zipWith :: (a -> b -> c) -> NonEmpty a -> NonEmpty b -> NonEmpty c
zipWith f (x :| xs) (y :| ys) = f x y :| P.zipWith f xs ys

filter :: (a -> Bool) -> NonEmpty a -> [a]
filter p = P.filter p . toList

partition :: (a -> Bool) -> NonEmpty a -> ([a], [a])
partition p = L.partition p . toList

takeWhile, dropWhile :: (a -> Bool) -> NonEmpty a -> [a]
takeWhile p = P.takeWhile p . toList
dropWhile p = P.dropWhile p . toList

take, drop :: Int -> NonEmpty a -> [a]
take n = P.take n . toList
drop n = P.drop n . toList

(!!) :: NonEmpty a -> Int -> a
(!!) xs n = toList xs P.!! n

group :: Eq a => [a] -> [NonEmpty a]
group = groupBy (==)

groupBy :: (a -> a -> Bool) -> [a] -> [NonEmpty a]
groupBy eq xs = P.map fromList (L.groupBy eq xs)

group1 :: Eq a => NonEmpty a -> NonEmpty (NonEmpty a)
group1 = fromList . group . toList

groupWith :: Eq b => (a -> b) -> [a] -> [NonEmpty a]
groupWith f = groupBy (\a b -> f a == f b)

groupAllWith :: Ord b => (a -> b) -> [a] -> [NonEmpty a]
groupAllWith f = groupWith f . L.sortBy (comparing f)

iterate :: (a -> a) -> a -> NonEmpty a
iterate f x = x :| P.iterate f (f x)

repeat :: a -> NonEmpty a
repeat x = x :| P.repeat x

cycle :: NonEmpty a -> NonEmpty a
cycle = lift P.cycle

scanl :: (b -> a -> b) -> b -> [a] -> NonEmpty b
scanl f z = fromList . P.scanl f z

scanr :: (a -> b -> b) -> b -> [a] -> NonEmpty b
scanr f z = fromList . P.scanr f z

xor :: NonEmpty Bool -> Bool
xor (x :| xs) = foldr (/=) x xs
