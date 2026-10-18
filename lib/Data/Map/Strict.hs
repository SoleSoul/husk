-- Data.Map.Strict: like Data.Map, but values are evaluated when inserted.
module Data.Map.Strict (
  Map, empty, singleton, fromList, fromListWith, fromListWithKey, fromAscList, fromDescList,
  fromDistinctAscList, null, size, lookup, member, notMember, findWithDefault, (!), (!?),
  lookupLT, lookupGT, lookupLE, lookupGE,
  insert, insertWith, insertWithKey, insertLookupWithKey, delete, adjust, adjustWithKey,
  update, updateWithKey, updateLookupWithKey, alter,
  lookupMin, lookupMax, findMin, findMax, deleteMin, deleteMax, minViewWithKey, maxViewWithKey,
  minView, maxView, deleteFindMin, deleteFindMax,
  union, unionWith, unionWithKey, unions, unionsWith, difference, (\\), differenceWith,
  intersection, intersectionWith, intersectionWithKey, disjoint, isSubmapOf, isSubmapOfBy,
  restrictKeys, withoutKeys,
  map, mapWithKey, mapKeys, mapKeysWith, mapMaybe, mapMaybeWithKey, mapAccum, mapAccumWithKey,
  traverseWithKey, filter, filterWithKey, partition, partitionWithKey,
  foldr, foldl, foldrWithKey, foldlWithKey, foldr', foldl', foldrWithKey', foldlWithKey',
  foldMapWithKey, elems, keys, assocs, toList, toAscList, toDescList,
  elemAt, lookupIndex, findIndex, deleteAt, take, drop, splitAt, split, splitLookup,
  takeWhileAntitone, dropWhileAntitone
  ) where
import Prelude (Ord, Maybe(..), seq, ($), (.))
import Husk.Base (listFoldl')
import Data.Map.Internal hiding (insert, insertWith, insertWithKey, fromList, fromListWith,
                                 fromListWithKey, adjust, alter, map, singleton)
import qualified Data.Map.Internal as L

singleton :: k -> a -> Map k a
singleton k x = x `seq` L.singleton k x

insert :: Ord k => k -> a -> Map k a -> Map k a
insert k x m = x `seq` L.insert k x m

insertWith :: Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWith = L.insertWith'

insertWithKey :: Ord k => (k -> a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWithKey f k = L.insertWith' (f k) k

fromList :: Ord k => [(k, a)] -> Map k a
fromList xs = listFoldl' (\m (k, x) -> insert k x m) L.empty xs

fromListWith :: Ord k => (a -> a -> a) -> [(k, a)] -> Map k a
fromListWith f xs = listFoldl' (\m (k, x) -> insertWith f k x m) L.empty xs

fromListWithKey :: Ord k => (k -> a -> a -> a) -> [(k, a)] -> Map k a
fromListWithKey f xs = listFoldl' (\m (k, x) -> insertWithKey f k x m) L.empty xs

adjust :: Ord k => (a -> a) -> k -> Map k a -> Map k a
adjust = L.adjust'

alter :: Ord k => (Maybe a -> Maybe a) -> k -> Map k a -> Map k a
alter = L.alter'

map :: (a -> b) -> Map k a -> Map k b
map = L.map'
