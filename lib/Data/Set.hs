-- Data.Set, as in the containers package.
-- Usually imported qualified:  import qualified Data.Set as Set
module Data.Set (
  Set, empty, singleton, fromList, fromAscList, fromDescList, fromDistinctAscList,
  null, size, member, notMember, lookupLT, lookupGT, lookupLE, lookupGE,
  insert, delete, lookupMin, lookupMax, findMin, findMax, deleteMin, deleteMax,
  minView, maxView, deleteFindMin, deleteFindMax,
  union, unions, difference, (\\), intersection, disjoint, isSubsetOf, isProperSubsetOf,
  filter, partition, map, mapMonotonic, foldr, foldl, foldr', foldl',
  elems, toList, toAscList, toDescList, elemAt, findIndex, lookupIndex,
  take, drop, splitAt, split, splitMember, cartesianProduct, powerSet
  ) where
import Prelude ()
import Data.Set.Internal
