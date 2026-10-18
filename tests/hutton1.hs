-- Exercises from Graham Hutton, "Programming in Haskell" (2nd ed.),
-- chapters 1-7, with my own solutions.
import Prelude hiding ((||), (&&))
import Data.Char

-- Chapter 1
product' :: Num a => [a] -> a
product' [] = 1
product' (n : ns) = n * product' ns

qsortRev :: Ord a => [a] -> [a]
qsortRev [] = []
qsortRev (x : xs) = qsortRev larger ++ [x] ++ qsortRev smaller
  where smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x]

qsortNoDups :: Ord a => [a] -> [a]
qsortNoDups [] = []
qsortNoDups (x : xs) = qsortNoDups [a | a <- xs, a < x] ++ [x] ++ qsortNoDups [b | b <- xs, b > x]

-- Chapter 2
n = a `div` length xs
  where a = 10
        xs = [1, 2, 3, 4, 5]

last1, last2 :: [a] -> a
last1 xs = head (reverse xs)
last2 xs = xs !! (length xs - 1)

init1, init2 :: [a] -> [a]
init1 xs = take (length xs - 1) xs
init2 = reverse . tail . reverse

-- Chapter 3
bools :: [Bool]
bools = [True, False]

nums :: [[Int]]
nums = [[1, 2], [3]]

add :: Int -> Int -> Int -> Int
add x y z = x + y + z

copy :: a -> (a, a)
copy x = (x, x)

apply :: (a -> b) -> a -> b
apply f x = f x

second xs = head (tail xs)
swap (x, y) = (y, x)
pair x y = (x, y)
double x = x * 2
palindrome xs = reverse xs == xs
twice f x = f (f x)

-- Chapter 4
halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

thirdA, thirdB, thirdC :: [a] -> a
thirdA xs = head (tail (tail xs))
thirdB xs = xs !! 2
thirdC (_ : _ : x : _) = x

safetailA, safetailB, safetailC :: [a] -> [a]
safetailA xs = if null xs then [] else tail xs
safetailB xs | null xs = []
             | otherwise = tail xs
safetailC [] = []
safetailC (_ : xs) = xs

infixr 2 ||
infixr 3 &&

(||) :: Bool -> Bool -> Bool
False || False = False
_ || _ = True

or2, or3, or4 :: Bool -> Bool -> Bool
or2 True True = True
or2 True False = True
or2 False True = True
or2 False False = False
or3 False b = b
or3 True _ = True
or4 b c | b == c = b
        | otherwise = True

(&&) :: Bool -> Bool -> Bool
a && b = if a then (if b then True else False) else False

and2 :: Bool -> Bool -> Bool
and2 a b = if a then b else False

mult :: Int -> Int -> Int -> Int
mult = \x -> (\y -> (\z -> x * y * z))

luhnDouble :: Int -> Int
luhnDouble x = if d > 9 then d - 9 else d where d = 2 * x

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = (luhnDouble a + b + luhnDouble c + d) `mod` 10 == 0

-- Chapter 5
grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x, y) | x <- [0 .. m], y <- [0 .. n]]

square :: Int -> [(Int, Int)]
square n = [(x, y) | (x, y) <- grid n n, x /= y]

replicate' :: Int -> a -> [a]
replicate' k x = [x | _ <- [1 .. k]]

pyths :: Int -> [(Int, Int, Int)]
pyths m = [(x, y, z) | x <- [1 .. m], y <- [1 .. m], z <- [1 .. m], x ^ 2 + y ^ 2 == z ^ 2]

factors :: Int -> [Int]
factors k = [x | x <- [1 .. k], k `mod` x == 0]

perfects :: Int -> [Int]
perfects m = [x | x <- [1 .. m], sum (factors x) - x == x]

find :: Eq a => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0 ..])

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]

shift :: Int -> Char -> Char
shift k c
  | isLower c = rot 'a'
  | isUpper c = rot 'A'
  | otherwise = c
  where rot base = chr (ord base + (ord c - ord base + k) `mod` 26)

encode :: Int -> String -> String
encode k xs = [shift k x | x <- xs]

-- Chapter 6
fac :: Int -> Int
fac 0 = 1
fac k | k > 0 = k * fac (k - 1)

sumdown :: Int -> Int
sumdown 0 = 0
sumdown k = k + sumdown (k - 1)

power :: Int -> Int -> Int
power _ 0 = 1
power m k = m * power m (k - 1)

euclid :: Int -> Int -> Int
euclid x y | x == y = x
           | x < y = euclid x (y - x)
           | otherwise = euclid (x - y) y

and' :: [Bool] -> Bool
and' [] = True
and' (b : bs) = b && and' bs

concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs : xss) = xs ++ concat' xss

replicate2 :: Int -> a -> [a]
replicate2 0 _ = []
replicate2 k x = x : replicate2 (k - 1) x

nth :: [a] -> Int -> a
nth (x : _) 0 = x
nth (_ : xs) k = nth xs (k - 1)

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x (y : ys) = x == y || elem' x ys

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys) | x <= y = x : merge xs (y : ys)
                        | otherwise = y : merge (x : xs) ys

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort ys) (msort zs) where (ys, zs) = halve xs

sum', lastR :: [Int] -> Int
sum' [] = 0
sum' (x : xs) = x + sum' xs
lastR [x] = x
lastR (_ : xs) = lastR xs

take' :: Int -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' k (x : xs) = x : take' (k - 1) xs

-- Chapter 7
all', any' :: (a -> Bool) -> [a] -> Bool
all' p = and . map p
any' p = or . map p

takeWhile', dropWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x : xs) | p x = x : takeWhile' p xs
                      | otherwise = []
dropWhile' _ [] = []
dropWhile' p (x : xs) | p x = dropWhile' p xs
                      | otherwise = x : xs

mapF :: (a -> b) -> [a] -> [b]
mapF f = foldr (\x xs -> f x : xs) []

filterF :: (a -> Bool) -> [a] -> [a]
filterF p = foldr (\x xs -> if p x then x : xs else xs) []

dec2int :: [Int] -> Int
dec2int = foldl (\v d -> 10 * v + d) 0

curry' :: ((a, b) -> c) -> a -> b -> c
curry' f x y = f (x, y)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (x, y) = f x y

unfold :: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> [a]
unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

type Bit = Int

int2bin :: Int -> [Bit]
int2bin = unfold (== 0) (`mod` 2) (`div` 2)

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2 * y) 0

chop8 :: [Bit] -> [[Bit]]
chop8 = unfold null (take 8) (drop 8)

mapU :: (a -> b) -> [a] -> [b]
mapU f = unfold null (f . head) tail

iterateU :: (a -> a) -> a -> [a]
iterateU f = unfold (const False) id f

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

addParity :: [Bit] -> [Bit]
addParity bits = (sum bits `mod` 2) : bits

checkParity :: [Bit] -> [Bit]
checkParity (p : bits) | p == sum bits `mod` 2 = bits
                       | otherwise = error "parity error"

chop9 :: [Bit] -> [[Bit]]
chop9 = unfold null (take 9) (drop 9)

encodeT :: String -> [Bit]
encodeT = concat . map (addParity . make8 . int2bin . ord)

decodeT :: [Bit] -> String
decodeT = map (chr . bin2int . checkParity) . chop9

transmit :: ([Bit] -> [Bit]) -> String -> String
transmit channel = decodeT . channel . encodeT

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f g (x : xs) = f x : altMap g f xs

luhnList :: [Int] -> Bool
luhnList ds = sum (altMap id luhnDouble (reverse ds)) `mod` 10 == 0

main :: IO ()
main = do
  print (product' [2, 3, 4], qsortRev [3, 5, 1, 4, 2], qsortNoDups [2, 2, 3, 1, 1])
  print (n, last1 [1, 2, 3], last2 "abc", init1 [1, 2, 3], init2 "abcd")
  print (bools, nums, add 1 2 3, copy 'x', apply not True)
  print (second [1, 2, 3], swap (1, 'a'), pair True 'c', double 3.5, palindrome "racecar", twice (* 2) 3)
  print (halve [1, 2, 3, 4, 5, 6], map ($ [1, 2, 3, 4]) [thirdA, thirdB, thirdC])
  print (map ($ ([] :: [Int])) [safetailA, safetailB, safetailC], safetailC [1, 2, 3])
  print ([a || b | a <- [False, True], b <- [False, True]], [or2 a b == or3 a b && or3 a b == or4 a b | a <- [False, True], b <- [False, True]])
  print ([a && b | a <- [False, True], b <- [False, True]], [and2 a b | a <- [False, True], b <- [False, True]])
  print (mult 2 3 4, luhnDouble 3, luhnDouble 6, luhn 1 7 8 4, luhn 4 7 8 3)
  print (sum [x ^ 2 | x <- [1 .. 100]], grid 1 2, square 2, replicate' 3 True)
  print (pyths 10, perfects 500)
  print ([(x, y) | x <- [1, 2], y <- [3, 4]] == concat [[(x, y) | y <- [3, 4]] | x <- [1, 2]])
  print (positions False [True, False, True, False], scalarproduct [1, 2, 3] [4, 5, 6])
  putStrLn (encode 3 "Haskell is Fun") >> putStrLn (encode (-3) (encode 3 "Haskell is Fun"))
  print (fac 5, sumdown 3, power 2 3, euclid 6 27)
  print (and' [True, False], concat' [[1], [2, 3], []], replicate2 3 'x', nth "hello" 1, elem' 3 [1, 2, 3])
  print (merge [2, 5, 6] [1, 3, 4], msort [5, 2, 9, 1, 5, 6], sum' [1 .. 10], take' 3 "haskell", lastR [1, 2, 3])
  print (all' even [2, 4], any' odd [2, 4], takeWhile' (< 3) [1 ..], dropWhile' (< 3) [1 .. 6])
  print (mapF (+ 1) [1, 2, 3], filterF even [1 .. 10], dec2int [2, 3, 4, 5])
  print (curry' fst 1 2, uncurry' (+) (3, 4), int2bin 13, chop8 [1, 0, 1, 1, 0, 0, 0, 0, 1, 1], mapU (* 2) [1, 2, 3], take 5 (iterateU (* 2) 1))
  putStrLn (transmit id "higher-order functions are easy")
  print (altMap (+ 10) (+ 100) [0, 1, 2, 3, 4], luhnList [1, 7, 8, 4], luhnList [4, 7, 8, 3])
  putStrLn (transmit tail "higher-order functions are easy")
