import Data.List (sortBy, groupBy)
import Data.Function (on)

-- local function using the outer signature's constrained type variable
describe :: Show a => a -> [String]
describe x = map g [1, 2, 3]
  where g n = show x ++ "#" ++ show n

-- mutual recursion: one binding with a signature, one without
evens :: [a] -> [a]
evens (x:xs) = x : odds xs
evens [] = []
odds (_:xs) = evens xs
odds [] = []

-- let-polymorphism with class constraints, used at two types
pairShow :: (String, String)
pairShow = let sh v = "<" ++ show v ++ ">" in (sh (1 :: Int), sh True)

-- defaulting inside a where clause (monomorphism restriction)
area :: Double -> Double
area r = k * r * r
  where k = 3.14159

-- numeric literals flowing to Double through signatures
scale :: Double -> [Double]
scale f = map (* f) [1, 2, 3]

-- higher-order functions with constraints
applyAll :: [a -> a] -> a -> a
applyAll fs x = foldr ($) x fs

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy ((==) `on` f)

-- class method used at a different instance inside an instance
data Wrapper a = Wrapper [a]
instance Show a => Show (Wrapper a) where
  show (Wrapper xs) = "Wrapper of " ++ show (length xs) ++ ": " ++ show xs

-- polymorphic data with constrained functions
data Stack a = Stack [a]
push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x : xs)
top :: Stack a -> Maybe a
top (Stack []) = Nothing
top (Stack (x:_)) = Just x

-- point-free definitions
countIf :: (a -> Bool) -> [a] -> Int
countIf p = length . filter p

compose :: [a -> a] -> a -> a
compose = foldr (.) id

-- where-bound helper shared by guards
grade :: Int -> Char
grade s
  | pct >= 90 = 'A'
  | pct >= 80 = 'B'
  | otherwise = 'C'
  where pct = s * 100 `div` maxScore
        maxScore = 50

main :: IO ()
main = do
  print (describe 'q')
  print (evens "abcdefg", odds [1 .. 7 :: Int])
  print pairShow
  print (area 2, scale 0.5)
  print (applyAll [(+ 1), (* 2)] 5, compose [(+ 1), (* 2)] (21 :: Int))
  print (groupOn (`div` 10) [1, 5, 12, 18, 25 :: Int])
  print (Wrapper "hi", Wrapper [Just True])
  print (top (push 'x' (Stack "yz")), top (Stack [] :: Stack ()))
  print (countIf even [1 .. 10 :: Int], map grade [48, 41, 10])
  print (sortBy (compare `on` snd) [(1 :: Int, 'c'), (2, 'a'), (3, 'b')])
  let average xs = sum xs / fromIntegral (length xs)
  print (average [1, 2, 3, 4], average [0.5])
