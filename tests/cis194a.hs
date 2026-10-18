{-# LANGUAGE FlexibleInstances #-}
-- CIS 194 (Spring 2013) homework 1-6, with my own solutions.  The course's
-- support files are replaced by small versions written for this test.
import Data.List (transpose)
import qualified Data.Map as M
import Data.Char (isDigit, isSpace)

-- HW1
-- CIS 194 HW 1
toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . zipWith ($) (cycle [id, (* 2)]) . reverse

sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigits

validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther (toDigits n)) `mod` 10 == 0

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n - 1) a c b ++ [(a, b)] ++ hanoi (n - 1) c b a

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _ = []
hanoi4 1 a b _ _ = [(a, b)]
hanoi4 n a b c d = hanoi4 k a c b d ++ hanoi (n - k) a b d ++ hanoi4 k c b a d
  where k = n - round (sqrt (fromIntegral (2 * n + 1) :: Double)) + 1

-- HW2 (the types from the course's Log.hs)
data MessageType = Info | Warning | Error Int deriving (Show, Eq)
type TimeStamp = Int
data LogMessage = LogMessage MessageType TimeStamp String | Unknown String deriving (Show, Eq)
data MessageTree = Leaf | Node MessageTree LogMessage MessageTree deriving (Show, Eq)

parseMessage :: String -> LogMessage
parseMessage s = case words s of
  ("I" : t : rest) | [(ts, "")] <- reads t -> LogMessage Info ts (unwords rest)
  ("W" : t : rest) | [(ts, "")] <- reads t -> LogMessage Warning ts (unwords rest)
  ("E" : sev : t : rest) | [(sv, "")] <- reads sev, [(ts, "")] <- reads t -> LogMessage (Error sv) ts (unwords rest)
  _ -> Unknown s

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert m Leaf = Node Leaf m Leaf
insert m@(LogMessage _ ts _) (Node l n@(LogMessage _ ts' _) r)
  | ts < ts' = Node (insert m l) n r
  | otherwise = Node l n (insert m r)
insert _ t = t

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l m r) = inOrder l ++ [m] ++ inOrder r

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong ms = [s | LogMessage (Error sev) _ s <- inOrder (build ms), sev >= 50]

sampleLog :: String
sampleLog = unlines
  [ "I 6 Completed armadillo processing", "I 1 Nothing to report", "E 99 10 Flange failed!"
  , "I 4 Everything normal", "I 11 Initiating self-destruct sequence", "E 70 3 Way too many pickles"
  , "E 65 8 Bad pickle-flange interaction detected", "W 5 Flange is due for a check-up"
  , "I 7 Out for lunch, back in two time steps", "E 20 2 Too many pickles", "I 9 Back from lunch", "garbage line" ]

-- HW3
skips :: [a] -> [[a]]
skips xs = [[x | (i, x) <- zip [1 ..] xs, i `mod` n == 0] | n <- [1 .. length xs]]

localMaxima :: [Integer] -> [Integer]
localMaxima (a : b : c : r) = [b | b > a, b > c] ++ localMaxima (b : c : r)
localMaxima _ = []

histogram :: [Integer] -> String
histogram xs = unlines (reverse (takeWhile (any (== '*')) (transpose [replicate (count d) '*' ++ repeat ' ' | d <- [0 .. 9]]))) ++ "==========\n0123456789\n"
  where count d = length (filter (== d) xs)

-- HW4
fun1 :: [Integer] -> Integer
fun1 = product . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 = sum . filter even . takeWhile (> 1) . iterate (\n -> if even n then n `div` 2 else 3 * n + 1)

data Tree a = TLeaf | TNode Integer (Tree a) a (Tree a) deriving (Show, Eq)

height :: Tree a -> Integer
height TLeaf = -1
height (TNode h _ _ _) = h

foldTree :: [a] -> Tree a
foldTree = foldr ins TLeaf
  where
    ins x TLeaf = TNode 0 TLeaf x TLeaf
    ins x (TNode _ l y r)
      | height l <= height r = let l' = ins x l in TNode (1 + max (height l') (height r)) l' y r
      | otherwise = let r' = ins x r in TNode (1 + max (height l) (height r')) l y r'

balanced :: Tree a -> Bool
balanced TLeaf = True
balanced (TNode _ l _ r) = abs (height l - height r) <= 1 && balanced l && balanced r

xor :: [Bool] -> Bool
xor = foldr (/=) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\x g acc -> g (f acc x)) id xs base

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = [2 * k + 1 | k <- [1 .. n], k `notElem` removed]
  where removed = [i + j + 2 * i * j | (i, j) <- cartProd [1 .. n] [1 .. n], i <= j, i + j + 2 * i * j <= n]

-- HW5 (with a small expression parser in place of the course's Parser.hs)
data ExprT = Lit Integer | Add ExprT ExprT | Mul ExprT ExprT deriving (Show, Eq)

data StackExp = PushI Integer | SAdd | SMul deriving Show
type Program = [StackExp]

stackVM :: Program -> Maybe Integer
stackVM = go []
  where go [x] [] = Just x
        go st (PushI n : p) = go (n : st) p
        go (b : a : st) (SAdd : p) = go (a + b : st) p
        go (b : a : st) (SMul : p) = go (a * b : st) p
        go _ _ = Nothing

-- expr ::= term ('+' term)*, term ::= factor ('*' factor)*, factor ::= int | '(' expr ')'
parseExp :: (Integer -> a) -> (a -> a -> a) -> (a -> a -> a) -> String -> Maybe a
parseExp lit' add' mul' s = case expr (filter (not . isSpace) s) of
    Just (e, "") -> Just e
    _ -> Nothing
  where
    expr cs = term cs >>= more '+' term add'
    term cs = factor cs >>= more '*' factor mul'
    more o p f (acc, c : rest) | c == o = p rest >>= \(e, r) -> more o p f (f acc e, r)
    more _ _ _ (acc, rest) = Just (acc, rest)
    factor ('(' : rest) = case expr rest of
      Just (e, ')' : r) -> Just (e, r)
      _ -> Nothing
    factor ('-' : rest) = number rest >>= \(n, r) -> Just (lit' (negate n), r)
    factor cs = number cs >>= \(n, r) -> Just (lit' n, r)
    number cs = case span isDigit cs of
      ("", _) -> Nothing
      (ds, r) -> Just (read ds, r)

eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit Add Mul

class Expr a where
  lit :: Integer -> a
  add, mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

reify :: ExprT -> ExprT
reify = id

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (> 0)
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax a) (MinMax b) = MinMax (max a b)
  mul (MinMax a) (MinMax b) = MinMax (min a b)

instance Expr Mod7 where
  lit n = Mod7 (n `mod` 7)
  add (Mod7 a) (Mod7 b) = Mod7 ((a + b) `mod` 7)
  mul (Mod7 a) (Mod7 b) = Mod7 ((a * b) `mod` 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

instance Expr Program where
  lit n = [PushI n]
  add a b = a ++ b ++ [SAdd]
  mul a b = a ++ b ++ [SMul]

compile :: String -> Maybe Program
compile = parseExp lit add mul

class HasVars a where
  var :: String -> a

data VarExprT = VLit Integer | VAdd VarExprT VarExprT | VMul VarExprT VarExprT | Var String
  deriving (Show, Eq)

instance Expr VarExprT where
  lit = VLit
  add = VAdd
  mul = VMul

instance HasVars VarExprT where
  var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit n _ = Just n
  add a b m = (+) <$> a m <*> b m
  mul a b m = (*) <$> a m <*> b m

withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
withVars vs e = e (M.fromList vs)

-- HW6
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0 ..]

fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) ys = Cons x (interleaveStreams ys xs)

ruler :: Stream Integer
ruler = go 0 where go n = interleaveStreams (streamRepeat n) (go (n + 1))

x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
  fromInteger n = Cons n (streamRepeat 0)
  negate = streamMap negate
  Cons a as + Cons b bs = Cons (a + b) (as + bs)
  Cons a as * bb@(Cons b bs) = Cons (a * b) (streamMap (* a) bs + as * bb)

instance Fractional (Stream Integer) where
  Cons a as / Cons b bs = q where q = Cons (a `div` b) (streamMap (`div` b) (as - q * bs))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x * x)

data Matrix = Matrix Integer Integer Integer Integer deriving Show

instance Num Matrix where
  Matrix a b c d * Matrix e f g h = Matrix (a * e + b * g) (a * f + b * h) (c * e + d * g) (c * f + d * h)

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = let Matrix _ f _ _ = Matrix 1 1 1 0 ^ n in f

main :: IO ()
main = do
  print (toDigits 1234, toDigitsRev 1234, toDigits 0, toDigits (-17))
  print (doubleEveryOther [8, 7, 6, 5], doubleEveryOther [1, 2, 3])
  print (sumDigits [16, 7, 12, 5], validate 4012888888881881, validate 4012888888881882)
  print (hanoi 2 "a" "b" "c", length (hanoi 15 "a" "b" "c"), length (hanoi4 15 "a" "b" "c" "d"))
  print (parseMessage "E 2 562 help help", parseMessage "I 29 la la la", parseMessage "This is not in the right format")
  print (whatWentWrong (parse sampleLog))
  print (skips "ABCD", skips "hello!", skips [1 :: Int], skips [True, False], skips ([] :: [Int]))
  print (localMaxima [2, 9, 5, 6, 1], localMaxima [2, 3, 4, 1, 5], localMaxima [1, 2, 3, 4, 5])
  putStr (histogram [1, 4, 5, 4, 6, 6, 3, 4, 2, 4, 9])
  print (fun1 [1 .. 10], fun2 27)
  let t = foldTree "ABCDEFGHIJ"
  print (height t, balanced t)
  print (xor [False, True, False], map' (* 2) [1, 2, 3], myFoldl (-) 10 [1, 2, 3], sieveSundaram 50)
  print (eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)), evalStr "(2+3)*4", evalStr "2+3*4", evalStr "2+*4")
  print (testExp :: Maybe Integer, testExp :: Maybe Bool, testExp :: Maybe MinMax, testExp :: Maybe Mod7)
  print (compile "(3 * -4) + 5", fmap stackVM (compile "(3 * -4) + 5"))
  print (withVars [("x", 6)] $ add (lit 3) (var "x"), withVars [("x", 6)] $ add (lit 3) (var "y"))
  print (add (lit 3) (var "x") :: VarExprT)
  print (take 15 fibs2, ruler, fibs3)
  print (map fib4 [0 .. 20], fib4 200)
