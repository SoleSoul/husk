-- Data.Sequence: finite sequences with fast access at both ends, indexing,
-- splitting and concatenation (all O(log n)).  Implemented as size-balanced
-- trees indexed by position (the containers package uses finger trees).
-- Usually imported qualified:  import qualified Data.Sequence as Seq
--
-- The patterns Empty, x :<| rest and rest :|> x work as in GHC.
module Data.Sequence (
  Seq(Empty, (:<|), (:|>)), ViewL(..), ViewR(..),
  empty, singleton, fromList, replicate, (<|), (|>), (><), null, length,
  index, lookup, (!?), adjust, adjust', update, insertAt, deleteAt,
  take, drop, splitAt, reverse, viewl, viewr, filter, partition,
  spanl, breakl, takeWhileL, dropWhileL, takeWhileR, dropWhileR,
  findIndexL, elemIndexL, mapWithIndex, foldlWithIndex, foldrWithIndex,
  zip, zipWith, sort, sortBy, sortOn, unfoldr, iterateN, fromFunction, scanl,
  tails, inits, chunksOf
  ) where

import Prelude hiding (null, length, lookup, take, drop, splitAt, reverse, filter,
                       replicate, zip, zipWith, scanl)
import qualified Prelude as P
import qualified Data.List as L
import Husk.Base (listFoldl')

infixr 5 <|, :<|, :<, ><
infixl 5 |>, :|>, :>

data Seq a = Nil | Node !Int a !(Seq a) !(Seq a)

data ViewL a = EmptyL | a :< Seq a deriving (Eq, Ord, Show)
data ViewR a = EmptyR | Seq a :> a deriving (Eq, Ord, Show)

-- pattern synonyms: the matcher, then the builder
pattern Empty = matchEmpty empty
pattern (:<|) = matchFront (<|)
pattern (:|>) = matchBack (|>)

matchEmpty :: Seq a -> Bool
matchEmpty Nil = True
matchEmpty _ = False

matchFront :: Seq a -> Maybe (a, Seq a)
matchFront s = case viewl s of
  EmptyL -> Nothing
  x :< rest -> Just (x, rest)

matchBack :: Seq a -> Maybe (Seq a, a)
matchBack s = case viewr s of
  EmptyR -> Nothing
  rest :> x -> Just (rest, x)

------------------------------------------------------------------------

size :: Seq a -> Int
size Nil = 0
size (Node n _ _ _) = n

empty :: Seq a
empty = Nil

singleton :: a -> Seq a
singleton x = Node 1 x Nil Nil

fromList :: [a] -> Seq a
fromList xs = fst (build (P.length xs) xs)
  where
    build 0 ys = (Nil, ys)
    build n ys = let nl = (n - 1) `div` 2
                     (l, ys1) = build nl ys
                 in case ys1 of
                      (y : ys2) -> let (r, ys3) = build (n - 1 - nl) ys2 in (node y l r, ys3)

replicate :: Int -> a -> Seq a
replicate n x = fromList (P.replicate n x)

null :: Seq a -> Bool
null Nil = True
null _ = False

length :: Seq a -> Int
length = size

(<|) :: a -> Seq a -> Seq a
x <| Nil = singleton x
x <| Node _ y l r = balance y (x <| l) r

(|>) :: Seq a -> a -> Seq a
Nil |> x = singleton x
Node _ y l r |> x = balance y l (r |> x)

(><) :: Seq a -> Seq a -> Seq a
(><) = merge

index :: Seq a -> Int -> a
index s i = case lookup i s of
  Just x -> x
  Nothing -> error ("index out of bounds in call to: Data.Sequence.index " ++ show i)

lookup :: Int -> Seq a -> Maybe a
lookup _ Nil = Nothing
lookup i (Node _ x l r)
  | i < sl = lookup i l
  | i == sl = Just x
  | otherwise = lookup (i - sl - 1) r
  where sl = size l

(!?) :: Seq a -> Int -> Maybe a
s !? i = lookup i s

adjust :: (a -> a) -> Int -> Seq a -> Seq a
adjust _ _ Nil = Nil
adjust f i (Node n x l r)
  | i < sl = Node n x (adjust f i l) r
  | i == sl = Node n (f x) l r
  | otherwise = Node n x l (adjust f (i - sl - 1) r)
  where sl = size l

adjust' :: (a -> a) -> Int -> Seq a -> Seq a
adjust' f i s = case lookup i s of
  Nothing -> s
  Just x -> let y = f x in y `seq` update i y s

update :: Int -> a -> Seq a -> Seq a
update i y = adjust (const y) i

insertAt :: Int -> a -> Seq a -> Seq a
insertAt i x s
  | i <= 0 = x <| s
  | i >= size s = s |> x
  | otherwise = let (a, b) = splitAt i s in link x a b

deleteAt :: Int -> Seq a -> Seq a
deleteAt i s
  | i < 0 || i >= size s = s
  | otherwise = let (a, b) = splitAt i s in merge a (drop 1 b)

splitAt :: Int -> Seq a -> (Seq a, Seq a)
splitAt _ Nil = (Nil, Nil)
splitAt i t@(Node _ x l r)
  | i <= 0 = (Nil, t)
  | i < sl = let (ll, lr) = splitAt i l in (ll, link x lr r)
  | i == sl = (l, x <| r)
  | otherwise = let (rl, rr) = splitAt (i - sl - 1) r in (link x l rl, rr)
  where sl = size l

take :: Int -> Seq a -> Seq a
take i s = fst (splitAt i s)

drop :: Int -> Seq a -> Seq a
drop i s = snd (splitAt i s)

reverse :: Seq a -> Seq a
reverse Nil = Nil
reverse (Node n x l r) = Node n x (reverse r) (reverse l)

viewl :: Seq a -> ViewL a
viewl Nil = EmptyL
viewl t = let (x, rest) = deleteFindMin t in x :< rest

viewr :: Seq a -> ViewR a
viewr Nil = EmptyR
viewr t = let (x, rest) = deleteFindMax t in rest :> x

filter :: (a -> Bool) -> Seq a -> Seq a
filter p s = fromList (P.filter p (toListS s))

partition :: (a -> Bool) -> Seq a -> (Seq a, Seq a)
partition p s = let (a, b) = L.partition p (toListS s) in (fromList a, fromList b)

spanl :: (a -> Bool) -> Seq a -> (Seq a, Seq a)
spanl p s = let (a, b) = P.span p (toListS s) in (fromList a, fromList b)

breakl :: (a -> Bool) -> Seq a -> (Seq a, Seq a)
breakl p = spanl (not . p)

takeWhileL :: (a -> Bool) -> Seq a -> Seq a
takeWhileL p = fst . spanl p

dropWhileL :: (a -> Bool) -> Seq a -> Seq a
dropWhileL p = snd . spanl p

takeWhileR :: (a -> Bool) -> Seq a -> Seq a
takeWhileR p = reverse . takeWhileL p . reverse

dropWhileR :: (a -> Bool) -> Seq a -> Seq a
dropWhileR p = reverse . dropWhileL p . reverse

findIndexL :: (a -> Bool) -> Seq a -> Maybe Int
findIndexL p s = L.findIndex p (toListS s)

elemIndexL :: Eq a => a -> Seq a -> Maybe Int
elemIndexL x = findIndexL (== x)

mapWithIndex :: (Int -> a -> b) -> Seq a -> Seq b
mapWithIndex f s = go 0 s
  where go _ Nil = Nil
        go off (Node n x l r) = let sl = size l
                                in Node n (f (off + sl) x) (go off l) (go (off + sl + 1) r)

foldlWithIndex :: (b -> Int -> a -> b) -> b -> Seq a -> b
foldlWithIndex f z s = listFoldl' (\acc (i, x) -> f acc i x) z (P.zip [0 ..] (toListS s))

foldrWithIndex :: (Int -> a -> b -> b) -> b -> Seq a -> b
foldrWithIndex f z s = P.foldr (\(i, x) acc -> f i x acc) z (P.zip [0 ..] (toListS s))

zip :: Seq a -> Seq b -> Seq (a, b)
zip a b = fromList (P.zip (toListS a) (toListS b))

zipWith :: (a -> b -> c) -> Seq a -> Seq b -> Seq c
zipWith f a b = fromList (P.zipWith f (toListS a) (toListS b))

sort :: Ord a => Seq a -> Seq a
sort = fromList . L.sort . toListS

sortBy :: (a -> a -> Ordering) -> Seq a -> Seq a
sortBy f = fromList . L.sortBy f . toListS

sortOn :: Ord b => (a -> b) -> Seq a -> Seq a
sortOn f = fromList . L.sortOn f . toListS

unfoldr :: (b -> Maybe (a, b)) -> b -> Seq a
unfoldr f = fromList . L.unfoldr f

iterateN :: Int -> (a -> a) -> a -> Seq a
iterateN n f x = fromList (P.take n (P.iterate f x))

fromFunction :: Int -> (Int -> a) -> Seq a
fromFunction n f = fromList (P.map f [0 .. n - 1])

scanl :: (b -> a -> b) -> b -> Seq a -> Seq b
scanl f z s = fromList (P.scanl f z (toListS s))

tails :: Seq a -> Seq (Seq a)
tails s = fromList [drop i s | i <- [0 .. size s]]

inits :: Seq a -> Seq (Seq a)
inits s = fromList [take i s | i <- [0 .. size s]]

chunksOf :: Int -> Seq a -> Seq (Seq a)
chunksOf k s
  | null s = empty
  | otherwise = let (a, b) = splitAt k s in a <| chunksOf k b

toListS :: Seq a -> [a]
toListS s = go s []
  where go Nil acc = acc
        go (Node _ x l r) acc = go l (x : go r acc)

------------------------------------------------------------------------
-- balancing (as in Data.Map.Internal)

node :: a -> Seq a -> Seq a -> Seq a
node x l r = Node (size l + size r + 1) x l r

balance :: a -> Seq a -> Seq a -> Seq a
balance x l r
  | sl + sr <= 1 = Node (sl + sr + 1) x l r
  | sr > 3 * sl = rotateL x l r
  | sl > 3 * sr = rotateR x l r
  | otherwise = Node (sl + sr + 1) x l r
  where sl = size l
        sr = size r

rotateL :: a -> Seq a -> Seq a -> Seq a
rotateL x l r@(Node _ _ ly ry)
  | size ly < 2 * size ry = singleL x l r
  | otherwise = doubleL x l r

rotateR :: a -> Seq a -> Seq a -> Seq a
rotateR x l@(Node _ _ ly ry) r
  | size ry < 2 * size ly = singleR x l r
  | otherwise = doubleR x l r

singleL :: a -> Seq a -> Seq a -> Seq a
singleL x1 t1 (Node _ x2 t2 t3) = node x2 (node x1 t1 t2) t3

singleR :: a -> Seq a -> Seq a -> Seq a
singleR x1 (Node _ x2 t1 t2) t3 = node x2 t1 (node x1 t2 t3)

doubleL :: a -> Seq a -> Seq a -> Seq a
doubleL x1 t1 (Node _ x2 (Node _ x3 t2 t3) t4) = node x3 (node x1 t1 t2) (node x2 t3 t4)

doubleR :: a -> Seq a -> Seq a -> Seq a
doubleR x1 (Node _ x2 t1 (Node _ x3 t2 t3)) t4 = node x3 (node x2 t1 t2) (node x1 t3 t4)

link :: a -> Seq a -> Seq a -> Seq a
link x Nil r = x <| r
link x l Nil = l |> x
link x l@(Node sl y ly ry) r@(Node sr z lz rz)
  | 3 * sl < sr = balance z (link x l lz) rz
  | 3 * sr < sl = balance y ly (link x ry r)
  | otherwise = node x l r

merge :: Seq a -> Seq a -> Seq a
merge Nil r = r
merge l Nil = l
merge l@(Node sl x lx rx) r@(Node sr y ly ry)
  | 3 * sl < sr = balance y (merge l ly) ry
  | 3 * sr < sl = balance x lx (merge rx r)
  | otherwise = glue l r

glue :: Seq a -> Seq a -> Seq a
glue Nil r = r
glue l Nil = l
glue l r
  | size l > size r = let (m, l') = deleteFindMax l in balance m l' r
  | otherwise = let (m, r') = deleteFindMin r in balance m l r'

deleteFindMin :: Seq a -> (a, Seq a)
deleteFindMin (Node _ x Nil r) = (x, r)
deleteFindMin (Node _ x l r) = let (m, l') = deleteFindMin l in (m, balance x l' r)

deleteFindMax :: Seq a -> (a, Seq a)
deleteFindMax (Node _ x l Nil) = (x, l)
deleteFindMax (Node _ x l r) = let (m, r') = deleteFindMax r in (m, balance x l r')

------------------------------------------------------------------------

instance Eq a => Eq (Seq a) where
  a == b = size a == size b && toListS a == toListS b

instance Ord a => Ord (Seq a) where
  compare a b = compare (toListS a) (toListS b)

instance Show a => Show (Seq a) where
  showsPrec d s = showParen (d > 10) (showString "fromList " . shows (toListS s))

instance Functor Seq where
  fmap _ Nil = Nil
  fmap f (Node n x l r) = Node n (f x) (fmap f l) (fmap f r)

instance Foldable Seq where
  foldr f z s = P.foldr f z (toListS s)
  toList = toListS
  length = size
  null = null

instance Traversable Seq where
  traverse f s = fmap fromList (traverse f (toListS s))

instance Semigroup (Seq a) where
  (<>) = (><)

instance Monoid (Seq a) where
  mempty = empty

instance Applicative Seq where
  pure = singleton
  fs <*> xs = fromList (toListS fs <*> toListS xs)

instance Monad Seq where
  xs >>= f = fromList (concatMap (toListS . f) (toListS xs))
