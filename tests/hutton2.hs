-- Exercises from Graham Hutton, "Programming in Haskell" (2nd ed.),
-- chapters 8, 9, 12, 14 and 15, with my own solutions.
import Data.Char
import Data.List (sortOn)
import Data.Foldable (toList)

-- Chapter 8: declaring types and classes
data Nat = Zero | Succ Nat deriving Show

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult (Succ m) n = add n (mult m n)

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n - 1))

data STree a = SLeaf a | SNode (STree a) a (STree a)

occurs :: Ord a => a -> STree a -> Bool
occurs x (SLeaf y) = x == y
occurs x (SNode l y r) = case compare x y of
  EQ -> True
  LT -> occurs x l
  GT -> occurs x r

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

leaves :: Tree a -> Int
leaves (Leaf _) = 1
leaves (Node l r) = leaves l + leaves r

balanced :: Tree a -> Bool
balanced (Leaf _) = True
balanced (Node l r) = abs (leaves l - leaves r) <= 1 && balanced l && balanced r

balance :: [a] -> Tree a
balance [x] = Leaf x
balance xs = Node (balance ys) (balance zs) where (ys, zs) = splitAt (length xs `div` 2) xs

data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f _ (Val n) = f n
folde f g (Add x y) = g (folde f g x) (folde f g y)

eval :: Expr -> Int
eval = folde id (+)

size :: Expr -> Int
size = folde (const 1) (+)

data Option a = None | Some a

instance Eq a => Eq (Option a) where
  None == None = True
  Some x == Some y = x == y
  _ == _ = False

data List a = Nil | Cons a (List a)

instance Eq a => Eq (List a) where
  Nil == Nil = True
  Cons x xs == Cons y ys = x == y && xs == ys
  _ == _ = False

-- the tautology checker, with Or and Equiv
data Prop = Const Bool | Var Char | Not Prop | And Prop Prop | Or Prop Prop
          | Imply Prop Prop | Equiv Prop Prop

type Subst = [(Char, Bool)]

find :: Eq k => k -> [(k, v)] -> v
find k t = head [v | (k', v) <- t, k == k']

evalP :: Subst -> Prop -> Bool
evalP _ (Const b) = b
evalP s (Var x) = find x s
evalP s (Not p) = not (evalP s p)
evalP s (And p q) = evalP s p && evalP s q
evalP s (Or p q) = evalP s p || evalP s q
evalP s (Imply p q) = evalP s p <= evalP s q
evalP s (Equiv p q) = evalP s p == evalP s q

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Or p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Equiv p q) = vars p ++ vars q

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False :) bss ++ map (True :) bss where bss = bools (n - 1)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x : xs) = x : filter (/= x) (rmdups xs)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs)) where vs = rmdups (vars p)

isTaut :: Prop -> Bool
isTaut p = and [evalP s p | s <- substs p]

-- the abstract machine, with multiplication
data AExpr = AVal Int | AAdd AExpr AExpr | AMult AExpr AExpr

data Op = EVALA AExpr | ADD Int | EVALM AExpr | MUL Int

type Cont = [Op]

evalA :: AExpr -> Cont -> Int
evalA (AVal n) c = exec c n
evalA (AAdd x y) c = evalA x (EVALA y : c)
evalA (AMult x y) c = evalA x (EVALM y : c)

exec :: Cont -> Int -> Int
exec [] n = n
exec (EVALA y : c) n = evalA y (ADD n : c)
exec (ADD n : c) m = exec c (n + m)
exec (EVALM y : c) n = evalA y (MUL n : c)
exec (MUL n : c) m = exec c (n * m)

value :: AExpr -> Int
value e = evalA e []

-- Chapter 9: the countdown problem
data COp = CAdd | CSub | CMul | CDiv

instance Show COp where
  show CAdd = "+"
  show CSub = "-"
  show CMul = "*"
  show CDiv = "/"

valid :: COp -> Int -> Int -> Bool
valid CAdd x y = x <= y
valid CSub x y = x > y
valid CMul x y = x /= 1 && y /= 1 && x <= y
valid CDiv x y = y /= 1 && x `mod` y == 0

apply :: COp -> Int -> Int -> Int
apply CAdd x y = x + y
apply CSub x y = x - y
apply CMul x y = x * y
apply CDiv x y = x `div` y

data CExpr = CVal Int | CApp COp CExpr CExpr

instance Show CExpr where
  show (CVal n) = show n
  show (CApp o l r) = brak l ++ show o ++ brak r
    where brak (CVal n) = show n
          brak e = "(" ++ show e ++ ")"

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x : xs) = yss ++ map (x :) yss where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y : ys) = (x : y : ys) : map (y :) (interleave x ys)

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x : xs) = concat (map (interleave x) (perms xs))

choices :: [a] -> [[a]]
choices xs = [zs | ys <- subs xs, zs <- perms ys]

removeOne :: Eq a => a -> [a] -> [a]
removeOne _ [] = []
removeOne x (y : ys) | x == y = ys
                     | otherwise = y : removeOne x ys

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _ = True
isChoice (x : xs) ys = x `elem` ys && isChoice xs (removeOne x ys)

split :: [a] -> [([a], [a])]
split [] = []
split [_] = []
split (x : xs) = ([x], xs) : [(x : ls, rs) | (ls, rs) <- split xs]

type Result = (CExpr, Int)

results :: [Int] -> [Result]
results [] = []
results [n] = [(CVal n, n) | n > 0]
results ns = [res | (ls, rs) <- split ns, lx <- results ls, ry <- results rs, res <- combine lx ry]
  where combine (l, x) (r, y) = [(CApp o l r, apply o x y) | o <- [CAdd, CSub, CMul, CDiv], valid o x y]

solutions :: [Int] -> Int -> [CExpr]
solutions ns n = [e | ns' <- choices ns, (e, m) <- results ns', m == n]

-- all expressions over a list, valid or not (exercise 4, on a smaller list)
exprsAll :: [Int] -> [CExpr]
exprsAll [] = []
exprsAll [n] = [CVal n]
exprsAll ns = [CApp o l r | (ls, rs) <- split ns, l <- exprsAll ls, r <- exprsAll rs, o <- [CAdd, CSub, CMul, CDiv]]

evalC :: CExpr -> [Int]
evalC (CVal n) = [n | n > 0]
evalC (CApp o l r) = [apply o x y | x <- evalC l, y <- evalC r, valid o x y]

-- nearest solutions, simplest first (exercise 6)
nearest :: [Int] -> Int -> [CExpr]
nearest ns n = map fst (take 3 (sortOn snd [(e, (abs (m - n), sizeC e)) | ns' <- choices ns, (e, m) <- results ns']))
  where sizeC (CVal _) = 1
        sizeC (CApp _ l r) = sizeC l + sizeC r

-- Chapter 12: monads and more
data BTree a = BLeaf | BNode (BTree a) a (BTree a) deriving Show

instance Functor BTree where
  fmap _ BLeaf = BLeaf
  fmap g (BNode l x r) = BNode (fmap g l) (g x) (fmap g r)

newtype Fn a b = Fn (a -> b)

runFn :: Fn a b -> a -> b
runFn (Fn f) = f

instance Functor (Fn a) where
  fmap g (Fn h) = Fn (g . h)

instance Applicative (Fn a) where
  pure x = Fn (const x)
  Fn g <*> Fn h = Fn (\x -> g x (h x))

instance Monad (Fn a) where
  Fn h >>= f = Fn (\x -> runFn (f (h x)) x)

newtype Zip a = Z [a] deriving Show

instance Functor Zip where
  fmap g (Z xs) = Z (fmap g xs)

instance Applicative Zip where
  pure x = Z (repeat x)
  Z gs <*> Z xs = Z [g x | (g, x) <- zip gs xs]

data VExpr a = V a | VVal Int | VAdd (VExpr a) (VExpr a) deriving Show

instance Functor VExpr where
  fmap f (V x) = V (f x)
  fmap _ (VVal n) = VVal n
  fmap f (VAdd l r) = VAdd (fmap f l) (fmap f r)

instance Applicative VExpr where
  pure = V
  V f <*> e = fmap f e
  VVal n <*> _ = VVal n
  VAdd l r <*> e = VAdd (l <*> e) (r <*> e)

instance Monad VExpr where
  V x >>= f = f x
  VVal n >>= _ = VVal n
  VAdd l r >>= f = VAdd (l >>= f) (r >>= f)

type State = Int

newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) = st

instance Functor ST where
  fmap g st = do { x <- st; return (g x) }

instance Applicative ST where
  pure x = S (\s -> (x, s))
  stf <*> stx = do { f <- stf; x <- stx; return (f x) }

instance Monad ST where
  st >>= f = S (\s -> let (x, s') = app st s in app (f x) s')

fresh :: ST Int
fresh = S (\n -> (n, n + 1))

mlabel :: BTree a -> ST (BTree (a, Int))
mlabel BLeaf = return BLeaf
mlabel (BNode l x r) = do
  l' <- mlabel l
  n <- fresh
  r' <- mlabel r
  return (BNode l' (x, n) r')

-- Chapter 14: Foldables and friends
newtype Pair a b = Pair (a, b) deriving Show

instance (Semigroup a, Semigroup b) => Semigroup (Pair a b) where
  Pair (a, b) <> Pair (c, d) = Pair (a <> c, b <> d)

instance (Monoid a, Monoid b) => Monoid (Pair a b) where
  mempty = Pair (mempty, mempty)

newtype Fun a b = Fun (a -> b)

instance Semigroup b => Semigroup (Fun a b) where
  Fun f <> Fun g = Fun (\x -> f x <> g x)

instance Monoid b => Monoid (Fun a b) where
  mempty = Fun (const mempty)

instance Foldable Option where
  foldMap _ None = mempty
  foldMap f (Some x) = f x

instance Functor Option where
  fmap _ None = None
  fmap f (Some x) = Some (f x)

instance Traversable Option where
  traverse _ None = pure None
  traverse g (Some x) = Some <$> g x

instance Show a => Show (Option a) where
  show None = "None"
  show (Some x) = "Some " ++ show x

instance Foldable BTree where
  foldMap _ BLeaf = mempty
  foldMap f (BNode l x r) = foldMap f l <> f x <> foldMap f r

instance Traversable BTree where
  traverse _ BLeaf = pure BLeaf
  traverse g (BNode l x r) = BNode <$> traverse g l <*> g x <*> traverse g r

filterF :: Foldable t => (a -> Bool) -> t a -> [a]
filterF p = foldMap (\x -> if p x then [x] else [])

-- Chapter 15: lazy evaluation
fibs :: [Integer]
fibs = 0 : 1 : [x + y | (x, y) <- zip fibs (tail fibs)]

repeatT :: a -> BTree a
repeatT x = t where t = BNode t x t

takeT :: Int -> BTree a -> BTree a
takeT 0 _ = BLeaf
takeT _ BLeaf = BLeaf
takeT n (BNode l x r) = BNode (takeT (n - 1) l) x (takeT (n - 1) r)

replicateT :: Int -> a -> BTree a
replicateT n = takeT n . repeatT

sqroot :: Double -> Double
sqroot x = snd (head (dropWhile (\(a, b) -> abs (a - b) > 0.00001) (zip approx (tail approx))))
  where approx = iterate (\a -> (a + x / a) / 2) 1.0

main :: IO ()
main = do
  print (nat2int (mult (int2nat 3) (int2nat 4)), add (Succ Zero) (Succ Zero))
  let st = SNode (SNode (SLeaf 1) 3 (SLeaf 4)) 5 (SNode (SLeaf 6) 7 (SLeaf 9))
  print (map (`occurs` st) [1, 2, 5, 9, 10])
  print (balance [1 .. 5], balanced (balance "abcdefg"), balanced (Node (Leaf 1) (Node (Leaf 2) (Node (Leaf 3) (Leaf 4)))))
  let e = Add (Add (Val 2) (Val 3)) (Val 4)
  print (eval e, size e)
  print (Some 3 == Some 3, Some 'a' == None, Cons 1 (Cons 2 Nil) == Cons 1 (Cons 2 Nil), Cons 1 Nil == Nil)
  let p1 = Or (Var 'A') (Not (Var 'A'))
      p2 = Equiv (And (Var 'A') (Var 'B')) (And (Var 'B') (Var 'A'))
      p3 = Equiv (Var 'A') (Var 'B')
  print (map isTaut [p1, p2, p3])
  print (value (AAdd (AMult (AVal 2) (AVal 3)) (AVal 4)), value (AMult (AAdd (AVal 2) (AVal 3)) (AVal 4)))
  print (length (choices [1, 2, 3]), isChoice [1, 3] [3, 2, 1], isChoice [1, 1] [1, 2])
  print (head (solutions [1, 3, 7, 10, 25, 50] 765), length (solutions [1, 3, 7, 10, 25, 50] 765))
  print (length (concatMap exprsAll (choices [1, 2, 3, 4])), length (concatMap evalC (concatMap exprsAll (choices [1, 2, 3, 4]))))
  print (nearest [1, 3, 7, 10, 25, 50] 831)
  print (fmap (* 2) (BNode (BNode BLeaf 1 BLeaf) 2 BLeaf))
  print (runFn (fmap (+ 1) (Fn (* 2))) 5, runFn ((+) <$> Fn (* 2) <*> Fn (+ 10)) 3, runFn (Fn (* 2) >>= \a -> Fn (+ a)) 4)
  print (pure (+ 1) <*> Z [1, 2, 3], (+) <$> Z [1, 2, 3] <*> Z [10, 20])
  print (VAdd (V 'x') (VVal 1) >>= \v -> if v == 'x' then VAdd (VVal 2) (V "y") else VVal 0)
  print (fst (app (mlabel (BNode (BNode BLeaf 'a' BLeaf) 'b' (BNode BLeaf 'c' BLeaf))) 0))
  print (Pair ("ab", [1]) <> Pair ("cd", [2]), mempty :: Pair String [Int])
  let Fun h = Fun show <> Fun (\n -> replicate n '!') <> mempty in putStrLn (h 3)
  print (sum (Some 3), length None, traverse (\x -> if x > 0 then Just x else Nothing) (Some 3), traverse (\x -> [x, x + 1]) (Some 1))
  let t = BNode (BNode BLeaf 1 BLeaf) 2 (BNode BLeaf 3 BLeaf)
  print (sum t, toList t, maximum t, traverse (\x -> if x > 0 then Just (x * 10) else Nothing) t)
  print (filterF even [1 .. 10], filterF odd t)
  print (take 10 fibs, replicateT 2 'x', sqroot 2)
