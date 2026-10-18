import Data.List (sortBy, insertBy)
import Data.Ord (comparing)

class Container f where
  empty :: f a
  insert :: a -> f a -> f a
  toL :: f a -> [a]
  fromL :: [a] -> f a
  fromL = foldr insert empty

newtype Stack a = Stack [a]
instance Container Stack where
  empty = Stack []
  insert x (Stack xs) = Stack (x : xs)
  toL (Stack xs) = xs

class Pretty a where
  pretty :: a -> String

instance Pretty String where
  pretty s = "\"" ++ s ++ "\""

instance Pretty Int where
  pretty n = "#" ++ show n

instance Pretty a => Pretty (Maybe a) where
  pretty Nothing = "none"
  pretty (Just x) = "some " ++ pretty x

sameOrd :: Ord a => a -> a -> Bool
sameOrd x y = x == y && not (x < y)

data Box a = Box { unBox :: a, label :: String } deriving Show

data Point = Point { px :: Int, py :: Int } deriving (Show, Eq, Ord)

infixl 1 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x

dist :: Point -> Int
dist p = abs (px p) + abs (py p)
  where abs n | n < 0 = negate n
              | otherwise = n

shadowing :: Int -> Int
shadowing map = map + length [map]   -- a parameter named like a Prelude function

priorityQueue :: [(Int, String)] -> [String]
priorityQueue = map snd . foldr (insertBy (comparing fst)) []

data Color = Red | Green | Blue deriving (Show, Eq, Ord, Enum, Bounded)

next :: Color -> Color
next c | c == maxBound = minBound
       | otherwise = succ c

main :: IO ()
main = do
  print (toL (fromL [1, 2, 3] :: Stack Int))
  putStrLn (pretty "hi" ++ " " ++ pretty (5 :: Int) ++ " " ++ pretty (Just (Just (7 :: Int))))
  print (sameOrd 'a' 'a', sameOrd (1 :: Int) 2)
  let b = Box { unBox = [1, 2 :: Int], label = "nums" }
  print (b, unBox b, b { label = "changed" })
  print (Point (-1) 2, dist (Point (-3) 4), Point 1 2 < Point 1 3)
  print ([1, 2, 3] |> map (* 2) |> sum)
  print (shadowing 10)
  print (priorityQueue [(3, "c"), (1, "a"), (2, "b")])
  print (map next [minBound .. maxBound])
  let (q, r) = 17 `divMod` 5
      x `plus` y = x + y
      (<+>) a b = a ++ "+" ++ b
  print (q `plus` r, "x" <+> "y")
  print (show (2 ^ (3 :: Int)), fromIntegral (length "abc") / 2)
  let pairs = [(x, y) | x <- [1 .. 3], y <- [x .. 3], x + y == 4]
  print pairs
  print $ foldr (\x (evens, odds) -> if even x then (x : evens, odds) else (evens, x : odds)) ([], []) [1 .. 10]
  mapM_ (\(i, c) -> putStrLn (show i ++ ": " ++ show c)) (zip [1 :: Int ..] [Red ..])
