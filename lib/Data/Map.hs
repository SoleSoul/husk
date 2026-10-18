-- Data.Map (lazy in the values), as in the containers package.
-- Usually imported qualified:  import qualified Data.Map as Map
module Data.Map (
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
import Prelude ()
import Data.Map.Internal
