-- Data.Set.Internal: size-balanced binary trees of elements
module Data.Set.Internal where

import Prelude hiding (map, filter, foldr, foldl, null, take, drop, splitAt)
import qualified Prelude as P
import qualified Husk.Base as B
import Husk.Base (listFoldl')

infixl 9 \\

data Set a = Tip | Bin !Int !a !(Set a) !(Set a)

empty :: Set a
empty = Tip

singleton :: a -> Set a
singleton x = Bin 1 x Tip Tip

fromList :: Ord a => [a] -> Set a
fromList xs = listFoldl' (flip insert) Tip xs

fromAscList :: Ord a => [a] -> Set a
fromAscList = fromList

fromDescList :: Ord a => [a] -> Set a
fromDescList = fromList

fromDistinctAscList :: Ord a => [a] -> Set a
fromDistinctAscList = fromList

null :: Set a -> Bool
null Tip = True
null _ = False

size :: Set a -> Int
size Tip = 0
size (Bin n _ _ _) = n

member :: Ord a => a -> Set a -> Bool
member _ Tip = False
member x (Bin _ y l r) = case compare x y of
  LT -> member x l
  GT -> member x r
  EQ -> True

notMember :: Ord a => a -> Set a -> Bool
notMember x s = not (member x s)

lookupLT :: Ord a => a -> Set a -> Maybe a
lookupLT x s = goNothing s
  where goNothing Tip = Nothing
        goNothing (Bin _ y l r) | x <= y = goNothing l
                                | otherwise = goJust y r
        goJust best Tip = Just best
        goJust best (Bin _ y l r) | x <= y = goJust best l
                                  | otherwise = goJust y r

lookupGT :: Ord a => a -> Set a -> Maybe a
lookupGT x s = goNothing s
  where goNothing Tip = Nothing
        goNothing (Bin _ y l r) | x < y = goJust y l
                                | otherwise = goNothing r
        goJust best Tip = Just best
        goJust best (Bin _ y l r) | x < y = goJust y l
                                  | otherwise = goJust best r

lookupLE :: Ord a => a -> Set a -> Maybe a
lookupLE x s = if member x s then Just x else lookupLT x s

lookupGE :: Ord a => a -> Set a -> Maybe a
lookupGE x s = if member x s then Just x else lookupGT x s

insert :: Ord a => a -> Set a -> Set a
insert x Tip = singleton x
insert x t@(Bin sz y l r) = case compare x y of
  LT -> balance y (insert x l) r
  GT -> balance y l (insert x r)
  EQ -> Bin sz x l r

delete :: Ord a => a -> Set a -> Set a
delete _ Tip = Tip
delete x (Bin _ y l r) = case compare x y of
  LT -> balance y (delete x l) r
  GT -> balance y l (delete x r)
  EQ -> glue l r

lookupMin :: Set a -> Maybe a
lookupMin Tip = Nothing
lookupMin (Bin _ x Tip _) = Just x
lookupMin (Bin _ _ l _) = lookupMin l

lookupMax :: Set a -> Maybe a
lookupMax Tip = Nothing
lookupMax (Bin _ x _ Tip) = Just x
lookupMax (Bin _ _ _ r) = lookupMax r

findMin :: Set a -> a
findMin s = case lookupMin s of
  Just x -> x
  Nothing -> error "Set.findMin: empty set has no minimal element"

findMax :: Set a -> a
findMax s = case lookupMax s of
  Just x -> x
  Nothing -> error "Set.findMax: empty set has no maximal element"

deleteMin :: Set a -> Set a
deleteMin Tip = Tip
deleteMin (Bin _ _ Tip r) = r
deleteMin (Bin _ x l r) = balance x (deleteMin l) r

deleteMax :: Set a -> Set a
deleteMax Tip = Tip
deleteMax (Bin _ _ l Tip) = l
deleteMax (Bin _ x l r) = balance x l (deleteMax r)

minView :: Set a -> Maybe (a, Set a)
minView Tip = Nothing
minView t = let (m, t') = deleteFindMin' t in Just (m, t')

maxView :: Set a -> Maybe (a, Set a)
maxView Tip = Nothing
maxView t = let (m, t') = deleteFindMax' t in Just (m, t')

deleteFindMin :: Set a -> (a, Set a)
deleteFindMin t = case minView t of
  Just r -> r
  Nothing -> (error "Set.deleteFindMin: can not return the minimal element of an empty set", Tip)

deleteFindMax :: Set a -> (a, Set a)
deleteFindMax t = case maxView t of
  Just r -> r
  Nothing -> (error "Set.deleteFindMax: can not return the maximal element of an empty set", Tip)

union :: Ord a => Set a -> Set a -> Set a
union t1 Tip = t1
union Tip t2 = t2
union (Bin _ x l1 r1) t2 = case split x t2 of
  (l2, r2) -> link x (union l1 l2) (union r1 r2)

unions :: (Foldable f, Ord a) => f (Set a) -> Set a
unions ss = listFoldl' union empty (B.toList ss)

difference :: Ord a => Set a -> Set a -> Set a
difference Tip _ = Tip
difference t1 Tip = t1
difference t1 (Bin _ x l2 r2) = case split x t1 of
  (l1, r1) -> merge (difference l1 l2) (difference r1 r2)

(\\) :: Ord a => Set a -> Set a -> Set a
m1 \\ m2 = difference m1 m2

intersection :: Ord a => Set a -> Set a -> Set a
intersection Tip _ = Tip
intersection _ Tip = Tip
intersection (Bin _ x l1 r1) t2 = case splitMember x t2 of
  (l2, found, r2) -> let l = intersection l1 l2
                         r = intersection r1 r2
                     in if found then link x l r else merge l r

disjoint :: Ord a => Set a -> Set a -> Bool
disjoint a b = null (intersection a b)

isSubsetOf :: Ord a => Set a -> Set a -> Bool
isSubsetOf a b = size a <= size b && all (\x -> member x b) (toAscList a)

isProperSubsetOf :: Ord a => Set a -> Set a -> Bool
isProperSubsetOf a b = size a < size b && isSubsetOf a b

filter :: (a -> Bool) -> Set a -> Set a
filter _ Tip = Tip
filter p (Bin _ x l r)
  | p x = link x (filter p l) (filter p r)
  | otherwise = merge (filter p l) (filter p r)

partition :: (a -> Bool) -> Set a -> (Set a, Set a)
partition p s = (filter p s, filter (not . p) s)

map :: Ord b => (a -> b) -> Set a -> Set b
map f s = fromList (P.map f (toAscList s))

mapMonotonic :: (a -> b) -> Set a -> Set b
mapMonotonic _ Tip = Tip
mapMonotonic f (Bin sz x l r) = Bin sz (f x) (mapMonotonic f l) (mapMonotonic f r)

foldr :: (a -> b -> b) -> b -> Set a -> b
foldr f z = go z
  where go z' Tip = z'
        go z' (Bin _ x l r) = go (f x (go z' r)) l

foldl :: (b -> a -> b) -> b -> Set a -> b
foldl f z = go z
  where go z' Tip = z'
        go z' (Bin _ x l r) = go (f (go z' l) x) r

foldr' :: (a -> b -> b) -> b -> Set a -> b
foldr' f z s = P.foldr (\x acc -> acc `seq` f x acc) z (toAscList s)

foldl' :: (b -> a -> b) -> b -> Set a -> b
foldl' f z = go z
  where go !z' Tip = z'
        go !z' (Bin _ x l r) = let z'' = go z' l in z'' `seq` go (f z'' x) r

elems :: Set a -> [a]
elems = toAscList

toList :: Set a -> [a]
toList = toAscList

toAscList :: Set a -> [a]
toAscList = foldr (:) []

toDescList :: Set a -> [a]
toDescList = foldl (flip (:)) []

elemAt :: Int -> Set a -> a
elemAt _ Tip = error "Set.elemAt: index out of range"
elemAt i (Bin _ x l r)
  | i < sl = elemAt i l
  | i > sl = elemAt (i - sl - 1) r
  | otherwise = x
  where sl = size l

findIndex :: Ord a => a -> Set a -> Int
findIndex = go 0
  where go _ _ Tip = error "Set.findIndex: element is not in the set"
        go idx x (Bin _ kx l r) = case compare x kx of
          LT -> go idx x l
          GT -> go (idx + size l + 1) x r
          EQ -> idx + size l

lookupIndex :: Ord a => a -> Set a -> Maybe Int
lookupIndex x s = if member x s then Just (findIndex x s) else Nothing

take :: Int -> Set a -> Set a
take n s = fromDistinct (P.take n (toAscList s))

drop :: Int -> Set a -> Set a
drop n s = fromDistinct (P.drop n (toAscList s))

splitAt :: Int -> Set a -> (Set a, Set a)
splitAt n s = (take n s, drop n s)

fromDistinct :: [a] -> Set a
fromDistinct xs = fst (go (length xs) xs)
  where
    go 0 ys = (Tip, ys)
    go n ys = let nl = (n - 1) `div` 2
                  (l, ys1) = go nl ys
              in case ys1 of
                   (x : ys2) -> let (r, ys3) = go (n - 1 - nl) ys2 in (bin x l r, ys3)

split :: Ord a => a -> Set a -> (Set a, Set a)
split _ Tip = (Tip, Tip)
split x (Bin _ y l r) = case compare x y of
  LT -> let (lt, gt) = split x l in (lt, link y gt r)
  GT -> let (lt, gt) = split x r in (link y l lt, gt)
  EQ -> (l, r)

splitMember :: Ord a => a -> Set a -> (Set a, Bool, Set a)
splitMember _ Tip = (Tip, False, Tip)
splitMember x (Bin _ y l r) = case compare x y of
  LT -> let (lt, found, gt) = splitMember x l in (lt, found, link y gt r)
  GT -> let (lt, found, gt) = splitMember x r in (link y l lt, found, gt)
  EQ -> (l, True, r)

cartesianProduct :: Set a -> Set b -> Set (a, b)
cartesianProduct as bs = fromDistinct [(a, b) | a <- toAscList as, b <- toAscList bs]

powerSet :: Set a -> Set (Set a)
powerSet s = fromDistinct (P.map fromDistinct (tail (subsequencesAsc (toAscList s))))
  where subsequencesAsc xs = [] : nonEmpty xs
        nonEmpty [] = []
        nonEmpty (y:ys) = let rest = nonEmpty ys in [y] : P.map (y:) rest ++ rest

-- balancing

delta, ratio :: Int
delta = 3
ratio = 2

bin :: a -> Set a -> Set a -> Set a
bin x l r = Bin (size l + size r + 1) x l r

balance :: a -> Set a -> Set a -> Set a
balance x l r
  | sl + sr <= 1 = Bin (sl + sr + 1) x l r
  | sr > delta * sl = rotateL x l r
  | sl > delta * sr = rotateR x l r
  | otherwise = Bin (sl + sr + 1) x l r
  where sl = size l
        sr = size r

rotateL :: a -> Set a -> Set a -> Set a
rotateL x l r@(Bin _ _ ly ry)
  | size ly < ratio * size ry = singleL x l r
  | otherwise = doubleL x l r

rotateR :: a -> Set a -> Set a -> Set a
rotateR x l@(Bin _ _ ly ry) r
  | size ry < ratio * size ly = singleR x l r
  | otherwise = doubleR x l r

singleL :: a -> Set a -> Set a -> Set a
singleL x1 t1 (Bin _ x2 t2 t3) = bin x2 (bin x1 t1 t2) t3

singleR :: a -> Set a -> Set a -> Set a
singleR x1 (Bin _ x2 t1 t2) t3 = bin x2 t1 (bin x1 t2 t3)

doubleL :: a -> Set a -> Set a -> Set a
doubleL x1 t1 (Bin _ x2 (Bin _ x3 t2 t3) t4) = bin x3 (bin x1 t1 t2) (bin x2 t3 t4)

doubleR :: a -> Set a -> Set a -> Set a
doubleR x1 (Bin _ x2 t1 (Bin _ x3 t2 t3)) t4 = bin x3 (bin x2 t1 t2) (bin x1 t3 t4)

link :: a -> Set a -> Set a -> Set a
link x Tip r = insertMin x r
link x l Tip = insertMax x l
link x l@(Bin sl y ly ry) r@(Bin sr z lz rz)
  | delta * sl < sr = balance z (link x l lz) rz
  | delta * sr < sl = balance y ly (link x ry r)
  | otherwise = bin x l r

insertMax :: a -> Set a -> Set a
insertMax x Tip = singleton x
insertMax x (Bin _ y l r) = balance y l (insertMax x r)

insertMin :: a -> Set a -> Set a
insertMin x Tip = singleton x
insertMin x (Bin _ y l r) = balance y (insertMin x l) r

merge :: Set a -> Set a -> Set a
merge Tip r = r
merge l Tip = l
merge l@(Bin sl x lx rx) r@(Bin sr y ly ry)
  | delta * sl < sr = balance y (merge l ly) ry
  | delta * sr < sl = balance x lx (merge rx r)
  | otherwise = glue l r

glue :: Set a -> Set a -> Set a
glue Tip r = r
glue l Tip = l
glue l r
  | size l > size r = let (m, l') = deleteFindMax' l in balance m l' r
  | otherwise = let (m, r') = deleteFindMin' r in balance m l r'

deleteFindMin' :: Set a -> (a, Set a)
deleteFindMin' (Bin _ x Tip r) = (x, r)
deleteFindMin' (Bin _ x l r) = let (m, l') = deleteFindMin' l in (m, balance x l' r)

deleteFindMax' :: Set a -> (a, Set a)
deleteFindMax' (Bin _ x l Tip) = (x, l)
deleteFindMax' (Bin _ x l r) = let (m, r') = deleteFindMax' r in (m, balance x l r')

instance Eq a => Eq (Set a) where
  s1 == s2 = size s1 == size s2 && toAscList s1 == toAscList s2

instance Ord a => Ord (Set a) where
  compare s1 s2 = compare (toAscList s1) (toAscList s2)

instance Show a => Show (Set a) where
  showsPrec d s = showParen (d > 10) (showString "fromList " . shows (toAscList s))

instance Foldable Set where
  foldr = foldr
  foldl = foldl
  foldl' = foldl'
  toList = toAscList
  length = size
  null = null
  elem x s = P.elem x (toAscList s)
  minimum = findMin
  maximum = findMax
  sum s = foldl' (+) 0 s
  product s = foldl' (*) 1 s

instance Ord a => Semigroup (Set a) where
  (<>) = union

instance Ord a => Monoid (Set a) where
  mempty = empty
