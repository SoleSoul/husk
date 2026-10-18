-- Data.Map.Internal: size-balanced binary trees (as in the containers
-- package).  Data.Map, Data.Map.Strict and Data.IntMap are built on this.
module Data.Map.Internal where

import Prelude hiding (lookup, map, filter, foldr, foldl, null, take, drop, splitAt)
import qualified Prelude as P
import qualified Husk.Base as B
import Husk.Base (listFoldl', liftA3)

infixl 9 !, !?, \\

data Map k a = Tip | Bin !Int !k a !(Map k a) !(Map k a)

------------------------------------------------------------------------
-- Construction

empty :: Map k a
empty = Tip

singleton :: k -> a -> Map k a
singleton k x = Bin 1 k x Tip Tip

fromList :: Ord k => [(k, a)] -> Map k a
fromList xs = listFoldl' (\m (k, x) -> insert k x m) Tip xs

fromListWith :: Ord k => (a -> a -> a) -> [(k, a)] -> Map k a
fromListWith f xs = listFoldl' (\m (k, x) -> insertWith f k x m) Tip xs

fromListWithKey :: Ord k => (k -> a -> a -> a) -> [(k, a)] -> Map k a
fromListWithKey f xs = listFoldl' (\m (k, x) -> insertWithKey f k x m) Tip xs

fromAscList :: Ord k => [(k, a)] -> Map k a
fromAscList = fromList

fromDescList :: Ord k => [(k, a)] -> Map k a
fromDescList = fromList

fromDistinctAscList :: Ord k => [(k, a)] -> Map k a
fromDistinctAscList = fromList

fromSet :: (k -> a) -> [k] -> Map k a
fromSet f ks = fromDistinctAscListUnsafe [(k, f k) | k <- ks]

-- build a balanced tree from a sorted list without comparisons
fromDistinctAscListUnsafe :: [(k, a)] -> Map k a
fromDistinctAscListUnsafe xs = fst (go (length xs) xs)
  where
    go 0 ys = (Tip, ys)
    go n ys = let nl = (n - 1) `div` 2
                  (l, ys1) = go nl ys
              in case ys1 of
                   ((k, x) : ys2) -> let (r, ys3) = go (n - 1 - nl) ys2
                                     in (bin k x l r, ys3)

------------------------------------------------------------------------
-- Queries

null :: Map k a -> Bool
null Tip = True
null _ = False

size :: Map k a -> Int
size Tip = 0
size (Bin n _ _ _ _) = n

lookup :: Ord k => k -> Map k a -> Maybe a
lookup _ Tip = Nothing
lookup k (Bin _ kx x l r) = case compare k kx of
  LT -> lookup k l
  GT -> lookup k r
  EQ -> Just x

member :: Ord k => k -> Map k a -> Bool
member k m = case lookup k m of
  Nothing -> False
  Just _ -> True

notMember :: Ord k => k -> Map k a -> Bool
notMember k m = not (member k m)

findWithDefault :: Ord k => a -> k -> Map k a -> a
findWithDefault def k m = case lookup k m of
  Nothing -> def
  Just x -> x

(!) :: Ord k => Map k a -> k -> a
m ! k = case lookup k m of
  Nothing -> error "Map.!: given key is not an element in the map"
  Just x -> x

(!?) :: Ord k => Map k a -> k -> Maybe a
m !? k = lookup k m

lookupLT :: Ord k => k -> Map k v -> Maybe (k, v)
lookupLT = goNothing
  where
    goNothing _ Tip = Nothing
    goNothing k (Bin _ kx x l r) | k <= kx = goNothing k l
                                 | otherwise = goJust k kx x r
    goJust _ kx' x' Tip = Just (kx', x')
    goJust k kx' x' (Bin _ kx x l r) | k <= kx = goJust k kx' x' l
                                     | otherwise = goJust k kx x r

lookupGT :: Ord k => k -> Map k v -> Maybe (k, v)
lookupGT = goNothing
  where
    goNothing _ Tip = Nothing
    goNothing k (Bin _ kx x l r) | k < kx = goJust k kx x l
                                 | otherwise = goNothing k r
    goJust _ kx' x' Tip = Just (kx', x')
    goJust k kx' x' (Bin _ kx x l r) | k < kx = goJust k kx x l
                                     | otherwise = goJust k kx' x' r

lookupLE :: Ord k => k -> Map k v -> Maybe (k, v)
lookupLE = goNothing
  where
    goNothing _ Tip = Nothing
    goNothing k (Bin _ kx x l r) = case compare k kx of
      LT -> goNothing k l
      EQ -> Just (kx, x)
      GT -> goJust k kx x r
    goJust _ kx' x' Tip = Just (kx', x')
    goJust k kx' x' (Bin _ kx x l r) = case compare k kx of
      LT -> goJust k kx' x' l
      EQ -> Just (kx, x)
      GT -> goJust k kx x r

lookupGE :: Ord k => k -> Map k v -> Maybe (k, v)
lookupGE = goNothing
  where
    goNothing _ Tip = Nothing
    goNothing k (Bin _ kx x l r) = case compare k kx of
      LT -> goJust k kx x l
      EQ -> Just (kx, x)
      GT -> goNothing k r
    goJust _ kx' x' Tip = Just (kx', x')
    goJust k kx' x' (Bin _ kx x l r) = case compare k kx of
      LT -> goJust k kx x l
      EQ -> Just (kx, x)
      GT -> goJust k kx' x' r

------------------------------------------------------------------------
-- Insertion and deletion

insert :: Ord k => k -> a -> Map k a -> Map k a
insert kx x Tip = singleton kx x
insert kx x (Bin sz ky y l r) = case compare kx ky of
  LT -> balance ky y (insert kx x l) r
  GT -> balance ky y l (insert kx x r)
  EQ -> Bin sz kx x l r

insertWith :: Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWith f = insertWithKey (\_ x' y' -> f x' y')

insertWithKey :: Ord k => (k -> a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWithKey _ kx x Tip = singleton kx x
insertWithKey f kx x (Bin sy ky y l r) = case compare kx ky of
  LT -> balance ky y (insertWithKey f kx x l) r
  GT -> balance ky y l (insertWithKey f kx x r)
  EQ -> Bin sy kx (f kx x y) l r

-- strict in the new value (used by Data.Map.Strict)
insertWith' :: Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWith' f kx x Tip = x `seq` singleton kx x
insertWith' f kx x (Bin sy ky y l r) = case compare kx ky of
  LT -> balance ky y (insertWith' f kx x l) r
  GT -> balance ky y l (insertWith' f kx x r)
  EQ -> let v = f x y in v `seq` Bin sy kx v l r

insertLookupWithKey :: Ord k => (k -> a -> a -> a) -> k -> a -> Map k a -> (Maybe a, Map k a)
insertLookupWithKey f k x m = (lookup k m, insertWithKey f k x m)

delete :: Ord k => k -> Map k a -> Map k a
delete _ Tip = Tip
delete k (Bin _ kx x l r) = case compare k kx of
  LT -> balance kx x (delete k l) r
  GT -> balance kx x l (delete k r)
  EQ -> glue l r

adjust :: Ord k => (a -> a) -> k -> Map k a -> Map k a
adjust f = adjustWithKey (\_ x -> f x)

adjustWithKey :: Ord k => (k -> a -> a) -> k -> Map k a -> Map k a
adjustWithKey _ _ Tip = Tip
adjustWithKey f k (Bin sx kx x l r) = case compare k kx of
  LT -> Bin sx kx x (adjustWithKey f k l) r
  GT -> Bin sx kx x l (adjustWithKey f k r)
  EQ -> Bin sx kx (f kx x) l r

adjust' :: Ord k => (a -> a) -> k -> Map k a -> Map k a
adjust' _ _ Tip = Tip
adjust' f k (Bin sx kx x l r) = case compare k kx of
  LT -> Bin sx kx x (adjust' f k l) r
  GT -> Bin sx kx x l (adjust' f k r)
  EQ -> let v = f x in v `seq` Bin sx kx v l r

update :: Ord k => (a -> Maybe a) -> k -> Map k a -> Map k a
update f = updateWithKey (\_ x -> f x)

updateWithKey :: Ord k => (k -> a -> Maybe a) -> k -> Map k a -> Map k a
updateWithKey _ _ Tip = Tip
updateWithKey f k (Bin sx kx x l r) = case compare k kx of
  LT -> balance kx x (updateWithKey f k l) r
  GT -> balance kx x l (updateWithKey f k r)
  EQ -> case f kx x of
          Just x' -> Bin sx kx x' l r
          Nothing -> glue l r

updateLookupWithKey :: Ord k => (k -> a -> Maybe a) -> k -> Map k a -> (Maybe a, Map k a)
updateLookupWithKey f k m = case lookup k m of
  Nothing -> (Nothing, m)
  Just x -> case f k x of
    Just x' -> (Just x', updateWithKey f k m)
    Nothing -> (Just x, delete k m)

alter :: Ord k => (Maybe a -> Maybe a) -> k -> Map k a -> Map k a
alter f k Tip = case f Nothing of
  Nothing -> Tip
  Just x -> singleton k x
alter f k (Bin sx kx x l r) = case compare k kx of
  LT -> balance kx x (alter f k l) r
  GT -> balance kx x l (alter f k r)
  EQ -> case f (Just x) of
          Just x' -> Bin sx kx x' l r
          Nothing -> glue l r

alter' :: Ord k => (Maybe a -> Maybe a) -> k -> Map k a -> Map k a
alter' f k m = case alter f k m of
  m' -> case lookup k m' of
          Just v -> v `seq` m'
          Nothing -> m'

------------------------------------------------------------------------
-- Min / max

lookupMin :: Map k a -> Maybe (k, a)
lookupMin Tip = Nothing
lookupMin (Bin _ k x Tip _) = Just (k, x)
lookupMin (Bin _ _ _ l _) = lookupMin l

lookupMax :: Map k a -> Maybe (k, a)
lookupMax Tip = Nothing
lookupMax (Bin _ k x _ Tip) = Just (k, x)
lookupMax (Bin _ _ _ _ r) = lookupMax r

findMin :: Map k a -> (k, a)
findMin m = case lookupMin m of
  Just r -> r
  Nothing -> error "Map.findMin: empty map has no minimal element"

findMax :: Map k a -> (k, a)
findMax m = case lookupMax m of
  Just r -> r
  Nothing -> error "Map.findMax: empty map has no maximal element"

deleteMin :: Map k a -> Map k a
deleteMin Tip = Tip
deleteMin (Bin _ _ _ Tip r) = r
deleteMin (Bin _ kx x l r) = balance kx x (deleteMin l) r

deleteMax :: Map k a -> Map k a
deleteMax Tip = Tip
deleteMax (Bin _ _ _ l Tip) = l
deleteMax (Bin _ kx x l r) = balance kx x l (deleteMax r)

minViewWithKey :: Map k a -> Maybe ((k, a), Map k a)
minViewWithKey Tip = Nothing
minViewWithKey t = let (k, x, t') = deleteFindMin' t in Just ((k, x), t')

maxViewWithKey :: Map k a -> Maybe ((k, a), Map k a)
maxViewWithKey Tip = Nothing
maxViewWithKey t = let (k, x, t') = deleteFindMax' t in Just ((k, x), t')

minView :: Map k a -> Maybe (a, Map k a)
minView t = case minViewWithKey t of
  Nothing -> Nothing
  Just ((_, x), t') -> Just (x, t')

maxView :: Map k a -> Maybe (a, Map k a)
maxView t = case maxViewWithKey t of
  Nothing -> Nothing
  Just ((_, x), t') -> Just (x, t')

deleteFindMin :: Map k a -> ((k, a), Map k a)
deleteFindMin t = case minViewWithKey t of
  Just r -> r
  Nothing -> (error "Map.deleteFindMin: can not return the minimal element of an empty map", Tip)

deleteFindMax :: Map k a -> ((k, a), Map k a)
deleteFindMax t = case maxViewWithKey t of
  Just r -> r
  Nothing -> (error "Map.deleteFindMax: can not return the maximal element of an empty map", Tip)

------------------------------------------------------------------------
-- Combining

union :: Ord k => Map k a -> Map k a -> Map k a
union t1 Tip = t1
union Tip t2 = t2
union (Bin _ k x l1 r1) t2 = case splitLookup k t2 of
  (l2, _, r2) -> link k x (union l1 l2) (union r1 r2)

unionWith :: Ord k => (a -> a -> a) -> Map k a -> Map k a -> Map k a
unionWith f = unionWithKey (\_ x y -> f x y)

unionWithKey :: Ord k => (k -> a -> a -> a) -> Map k a -> Map k a -> Map k a
unionWithKey _ t1 Tip = t1
unionWithKey _ Tip t2 = t2
unionWithKey f (Bin _ k x l1 r1) t2 = case splitLookup k t2 of
  (l2, mb, r2) -> let x' = case mb of
                             Nothing -> x
                             Just y -> f k x y
                  in link k x' (unionWithKey f l1 l2) (unionWithKey f r1 r2)

unions :: (Foldable f, Ord k) => f (Map k a) -> Map k a
unions ts = listFoldl' union empty (B.toList ts)

unionsWith :: (Foldable f, Ord k) => (a -> a -> a) -> f (Map k a) -> Map k a
unionsWith f ts = listFoldl' (unionWith f) empty (B.toList ts)

difference :: Ord k => Map k a -> Map k b -> Map k a
difference Tip _ = Tip
difference t1 Tip = t1
difference t1 (Bin _ k _ l2 r2) = case split k t1 of
  (l1, r1) -> merge (difference l1 l2) (difference r1 r2)

(\\) :: Ord k => Map k a -> Map k b -> Map k a
m1 \\ m2 = difference m1 m2

differenceWith :: Ord k => (a -> b -> Maybe a) -> Map k a -> Map k b -> Map k a
differenceWith f t1 t2 = mergeWithKeyList t1 t2
  where mergeWithKeyList a b = filterMapWithKey (\k x -> case lookup k b of
                                                          Nothing -> Just x
                                                          Just y -> f x y) a

intersection :: Ord k => Map k a -> Map k b -> Map k a
intersection = intersectionWith const

intersectionWith :: Ord k => (a -> b -> c) -> Map k a -> Map k b -> Map k c
intersectionWith f = intersectionWithKey (\_ x y -> f x y)

intersectionWithKey :: Ord k => (k -> a -> b -> c) -> Map k a -> Map k b -> Map k c
intersectionWithKey _ Tip _ = Tip
intersectionWithKey _ _ Tip = Tip
intersectionWithKey f (Bin _ k x1 l1 r1) t2 = case splitLookup k t2 of
  (l2, mb, r2) -> let l = intersectionWithKey f l1 l2
                      r = intersectionWithKey f r1 r2
                  in case mb of
                       Just x2 -> link k (f k x1 x2) l r
                       Nothing -> merge l r

disjoint :: Ord k => Map k a -> Map k b -> Bool
disjoint a b = null (intersection a b)

isSubmapOf :: (Ord k, Eq a) => Map k a -> Map k a -> Bool
isSubmapOf m1 m2 = isSubmapOfBy (==) m1 m2

isSubmapOfBy :: Ord k => (a -> b -> Bool) -> Map k a -> Map k b -> Bool
isSubmapOfBy f m1 m2 = all (\(k, x) -> case lookup k m2 of
                                          Nothing -> False
                                          Just y -> f x y) (toAscList m1)

restrictKeys :: Ord k => Map k a -> [k] -> Map k a
restrictKeys m ks = filterWithKey (\k _ -> k `P.elem` ks) m

withoutKeys :: Ord k => Map k a -> [k] -> Map k a
withoutKeys m ks = listFoldl' (\acc k -> delete k acc) m ks

------------------------------------------------------------------------
-- Traversals

map :: (a -> b) -> Map k a -> Map k b
map _ Tip = Tip
map f (Bin sx kx x l r) = Bin sx kx (f x) (map f l) (map f r)

map' :: (a -> b) -> Map k a -> Map k b
map' _ Tip = Tip
map' f (Bin sx kx x l r) = let v = f x in v `seq` Bin sx kx v (map' f l) (map' f r)

mapWithKey :: (k -> a -> b) -> Map k a -> Map k b
mapWithKey _ Tip = Tip
mapWithKey f (Bin sx kx x l r) = Bin sx kx (f kx x) (mapWithKey f l) (mapWithKey f r)

mapKeys :: Ord k2 => (k1 -> k2) -> Map k1 a -> Map k2 a
mapKeys f m = fromList [(f k, x) | (k, x) <- toAscList m]

mapKeysWith :: Ord k2 => (a -> a -> a) -> (k1 -> k2) -> Map k1 a -> Map k2 a
mapKeysWith c f m = fromListWith c [(f k, x) | (k, x) <- toAscList m]

mapMaybe :: (a -> Maybe b) -> Map k a -> Map k b
mapMaybe f = filterMapWithKey (\_ x -> f x)

mapMaybeWithKey :: (k -> a -> Maybe b) -> Map k a -> Map k b
mapMaybeWithKey = filterMapWithKey

filterMapWithKey :: (k -> a -> Maybe b) -> Map k a -> Map k b
filterMapWithKey _ Tip = Tip
filterMapWithKey f (Bin _ kx x l r) = case f kx x of
  Just y -> link kx y (filterMapWithKey f l) (filterMapWithKey f r)
  Nothing -> merge (filterMapWithKey f l) (filterMapWithKey f r)

mapAccum :: (a -> b -> (a, c)) -> a -> Map k b -> (a, Map k c)
mapAccum f = mapAccumWithKey (\a _ x -> f a x)

mapAccumWithKey :: (a -> k -> b -> (a, c)) -> a -> Map k b -> (a, Map k c)
mapAccumWithKey _ a Tip = (a, Tip)
mapAccumWithKey f a (Bin sx kx x l r) =
  let (a1, l') = mapAccumWithKey f a l
      (a2, x') = f a1 kx x
      (a3, r') = mapAccumWithKey f a2 r
  in (a3, Bin sx kx x' l' r')

traverseWithKey :: Applicative t => (k -> a -> t b) -> Map k a -> t (Map k b)
traverseWithKey _ Tip = pure Tip
traverseWithKey f (Bin s k v l r) =
  liftA3 (\l' v' r' -> Bin s k v' l' r') (traverseWithKey f l) (f k v) (traverseWithKey f r)

filter :: (a -> Bool) -> Map k a -> Map k a
filter p = filterWithKey (\_ x -> p x)

filterWithKey :: (k -> a -> Bool) -> Map k a -> Map k a
filterWithKey _ Tip = Tip
filterWithKey p (Bin _ kx x l r)
  | p kx x = link kx x (filterWithKey p l) (filterWithKey p r)
  | otherwise = merge (filterWithKey p l) (filterWithKey p r)

partition :: (a -> Bool) -> Map k a -> (Map k a, Map k a)
partition p = partitionWithKey (\_ x -> p x)

partitionWithKey :: (k -> a -> Bool) -> Map k a -> (Map k a, Map k a)
partitionWithKey p m = (filterWithKey p m, filterWithKey (\k x -> not (p k x)) m)

------------------------------------------------------------------------
-- Folds and conversion

foldr :: (a -> b -> b) -> b -> Map k a -> b
foldr f z = go z
  where go z' Tip = z'
        go z' (Bin _ _ x l r) = go (f x (go z' r)) l

foldl :: (b -> a -> b) -> b -> Map k a -> b
foldl f z = go z
  where go z' Tip = z'
        go z' (Bin _ _ x l r) = go (f (go z' l) x) r

foldrWithKey :: (k -> a -> b -> b) -> b -> Map k a -> b
foldrWithKey f z = go z
  where go z' Tip = z'
        go z' (Bin _ kx x l r) = go (f kx x (go z' r)) l

foldlWithKey :: (b -> k -> a -> b) -> b -> Map k a -> b
foldlWithKey f z = go z
  where go z' Tip = z'
        go z' (Bin _ kx x l r) = go (f (go z' l) kx x) r

foldr' :: (a -> b -> b) -> b -> Map k a -> b
foldr' f z = go z
  where go !z' Tip = z'
        go !z' (Bin _ _ x l r) = let z'' = go z' r in z'' `seq` go (f x z'') l

foldl' :: (b -> a -> b) -> b -> Map k a -> b
foldl' f z = go z
  where go !z' Tip = z'
        go !z' (Bin _ _ x l r) = let z'' = go z' l in z'' `seq` go (f z'' x) r

foldrWithKey' :: (k -> a -> b -> b) -> b -> Map k a -> b
foldrWithKey' f z m = P.foldr (\(k, x) acc -> acc `seq` f k x acc) z (toAscList m)

foldlWithKey' :: (b -> k -> a -> b) -> b -> Map k a -> b
foldlWithKey' f z = go z
  where go !z' Tip = z'
        go !z' (Bin _ kx x l r) = let z'' = go z' l in z'' `seq` go (f z'' kx x) r

foldMapWithKey :: Monoid m => (k -> a -> m) -> Map k a -> m
foldMapWithKey f = foldrWithKey (\k x acc -> f k x <> acc) mempty

elems :: Map k a -> [a]
elems = foldr (:) []

keys :: Map k a -> [k]
keys = foldrWithKey (\k _ ks -> k : ks) []

assocs :: Map k a -> [(k, a)]
assocs = toAscList

toList :: Map k a -> [(k, a)]
toList = toAscList

toAscList :: Map k a -> [(k, a)]
toAscList = foldrWithKey (\k x xs -> (k, x) : xs) []

toDescList :: Map k a -> [(k, a)]
toDescList = foldlWithKey (\xs k x -> (k, x) : xs) []

elemAt :: Int -> Map k a -> (k, a)
elemAt _ Tip = error "Map.elemAt: index out of range"
elemAt i (Bin _ kx x l r)
  | i < sl = elemAt i l
  | i > sl = elemAt (i - sl - 1) r
  | otherwise = (kx, x)
  where sl = size l

lookupIndex :: Ord k => k -> Map k a -> Maybe Int
lookupIndex = go 0
  where go _ _ Tip = Nothing
        go idx k (Bin _ kx _ l r) = case compare k kx of
          LT -> go idx k l
          GT -> go (idx + size l + 1) k r
          EQ -> Just (idx + size l)

findIndex :: Ord k => k -> Map k a -> Int
findIndex k m = case lookupIndex k m of
  Just i -> i
  Nothing -> error "Map.findIndex: element is not in the map"

deleteAt :: Int -> Map k a -> Map k a
deleteAt i t = case t of
  Tip -> error "Map.deleteAt: index out of range"
  Bin _ kx x l r -> let sl = size l in
    if i < sl then balance kx x (deleteAt i l) r
    else if i > sl then balance kx x l (deleteAt (i - sl - 1) r)
    else glue l r

take :: Int -> Map k a -> Map k a
take n m = fromDistinctAscListUnsafe (P.take n (toAscList m))

drop :: Int -> Map k a -> Map k a
drop n m = fromDistinctAscListUnsafe (P.drop n (toAscList m))

splitAt :: Int -> Map k a -> (Map k a, Map k a)
splitAt n m = (take n m, drop n m)

split :: Ord k => k -> Map k a -> (Map k a, Map k a)
split _ Tip = (Tip, Tip)
split k (Bin _ kx x l r) = case compare k kx of
  LT -> let (lt, gt) = split k l in (lt, link kx x gt r)
  GT -> let (lt, gt) = split k r in (link kx x l lt, gt)
  EQ -> (l, r)

splitLookup :: Ord k => k -> Map k a -> (Map k a, Maybe a, Map k a)
splitLookup _ Tip = (Tip, Nothing, Tip)
splitLookup k (Bin _ kx x l r) = case compare k kx of
  LT -> let (lt, z, gt) = splitLookup k l in (lt, z, link kx x gt r)
  GT -> let (lt, z, gt) = splitLookup k r in (link kx x l lt, z, gt)
  EQ -> (l, Just x, r)

takeWhileAntitone :: (k -> Bool) -> Map k a -> Map k a
takeWhileAntitone p m = fromDistinctAscListUnsafe (P.takeWhile (p . fst) (toAscList m))

dropWhileAntitone :: (k -> Bool) -> Map k a -> Map k a
dropWhileAntitone p m = fromDistinctAscListUnsafe (P.dropWhile (p . fst) (toAscList m))

------------------------------------------------------------------------
-- Balancing (Adams' weight-balanced trees)

delta, ratio :: Int
delta = 3
ratio = 2

bin :: k -> a -> Map k a -> Map k a -> Map k a
bin k x l r = Bin (size l + size r + 1) k x l r

balance :: k -> a -> Map k a -> Map k a -> Map k a
balance k x l r
  | sizeL + sizeR <= 1 = Bin sizeX k x l r
  | sizeR > delta * sizeL = rotateL k x l r
  | sizeL > delta * sizeR = rotateR k x l r
  | otherwise = Bin sizeX k x l r
  where sizeL = size l
        sizeR = size r
        sizeX = sizeL + sizeR + 1

rotateL :: k -> a -> Map k a -> Map k a -> Map k a
rotateL k x l r@(Bin _ _ _ ly ry)
  | size ly < ratio * size ry = singleL k x l r
  | otherwise = doubleL k x l r

rotateR :: k -> a -> Map k a -> Map k a -> Map k a
rotateR k x l@(Bin _ _ _ ly ry) r
  | size ry < ratio * size ly = singleR k x l r
  | otherwise = doubleR k x l r

singleL :: k -> a -> Map k a -> Map k a -> Map k a
singleL k1 x1 t1 (Bin _ k2 x2 t2 t3) = bin k2 x2 (bin k1 x1 t1 t2) t3

singleR :: k -> a -> Map k a -> Map k a -> Map k a
singleR k1 x1 (Bin _ k2 x2 t1 t2) t3 = bin k2 x2 t1 (bin k1 x1 t2 t3)

doubleL :: k -> a -> Map k a -> Map k a -> Map k a
doubleL k1 x1 t1 (Bin _ k2 x2 (Bin _ k3 x3 t2 t3) t4) = bin k3 x3 (bin k1 x1 t1 t2) (bin k2 x2 t3 t4)

doubleR :: k -> a -> Map k a -> Map k a -> Map k a
doubleR k1 x1 (Bin _ k2 x2 t1 (Bin _ k3 x3 t2 t3)) t4 = bin k3 x3 (bin k2 x2 t1 t2) (bin k1 x1 t3 t4)

link :: k -> a -> Map k a -> Map k a -> Map k a
link kx x Tip r = insertMin kx x r
link kx x l Tip = insertMax kx x l
link kx x l@(Bin sizeL ky y ly ry) r@(Bin sizeR kz z lz rz)
  | delta * sizeL < sizeR = balance kz z (link kx x l lz) rz
  | delta * sizeR < sizeL = balance ky y ly (link kx x ry r)
  | otherwise = bin kx x l r

insertMax :: k -> a -> Map k a -> Map k a
insertMax kx x t = case t of
  Tip -> singleton kx x
  Bin _ ky y l r -> balance ky y l (insertMax kx x r)

insertMin :: k -> a -> Map k a -> Map k a
insertMin kx x t = case t of
  Tip -> singleton kx x
  Bin _ ky y l r -> balance ky y (insertMin kx x l) r

merge :: Map k a -> Map k a -> Map k a
merge Tip r = r
merge l Tip = l
merge l@(Bin sizeL kx x lx rx) r@(Bin sizeR ky y ly ry)
  | delta * sizeL < sizeR = balance ky y (merge l ly) ry
  | delta * sizeR < sizeL = balance kx x lx (merge rx r)
  | otherwise = glue l r

glue :: Map k a -> Map k a -> Map k a
glue Tip r = r
glue l Tip = l
glue l r
  | size l > size r = let (km, m, l') = deleteFindMax' l in balance km m l' r
  | otherwise = let (km, m, r') = deleteFindMin' r in balance km m l r'

deleteFindMin' :: Map k a -> (k, a, Map k a)
deleteFindMin' (Bin _ k x Tip r) = (k, x, r)
deleteFindMin' (Bin _ k x l r) = let (km, m, l') = deleteFindMin' l in (km, m, balance k x l' r)

deleteFindMax' :: Map k a -> (k, a, Map k a)
deleteFindMax' (Bin _ k x l Tip) = (k, x, l)
deleteFindMax' (Bin _ k x l r) = let (km, m, r') = deleteFindMax' r in (km, m, balance k x l r')

------------------------------------------------------------------------
-- Instances

instance (Eq k, Eq a) => Eq (Map k a) where
  t1 == t2 = size t1 == size t2 && toAscList t1 == toAscList t2

instance (Ord k, Ord v) => Ord (Map k v) where
  compare m1 m2 = compare (toAscList m1) (toAscList m2)

instance (Show k, Show a) => Show (Map k a) where
  showsPrec d m = showParen (d > 10) (showString "fromList " . shows (toAscList m))

instance Functor (Map k) where
  fmap = map

instance Foldable (Map k) where
  foldr = foldr
  foldl = foldl
  foldr' = foldr'
  foldl' = foldl'
  toList = elems
  length = size
  null = null
  elem x m = P.elem x (elems m)
  sum m = foldl' (+) 0 m
  product m = foldl' (*) 1 m

instance Traversable (Map k) where
  traverse f = traverseWithKey (\_ x -> f x)

instance Ord k => Semigroup (Map k v) where
  (<>) = union

instance Ord k => Monoid (Map k v) where
  mempty = empty
