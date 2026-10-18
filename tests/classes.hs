import Data.List (intercalate)

-- A user-defined State monad
newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap f (State g) = State $ \s -> let (a, s') = g s in (f a, s')

instance Applicative (State s) where
  pure a = State $ \s -> (a, s)
  State f <*> State g = State $ \s ->
    let (h, s1) = f s
        (a, s2) = g s1
    in (h a, s2)

instance Monad (State s) where
  return = pure
  State g >>= k = State $ \s -> let (a, s') = g s in runState (k a) s'

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

modify :: (s -> s) -> State s ()
modify f = get >>= put . f

evalState :: State s a -> s -> a
evalState m s = fst (runState m s)

counter :: State Int [Int]
counter = do
  a <- get
  put (a + 1)
  b <- get
  modify (* 10)
  c <- get
  return [a, b, c]

labelTree :: Tree a -> State Int (Tree (Int, a))
labelTree (Leaf x) = do
  n <- get
  put (n + 1)
  return (Leaf (n, x))
labelTree (Branch l r) = Branch <$> labelTree l <*> labelTree r

data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving Show

instance Functor Tree where
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Branch l r) = Branch (fmap f l) (fmap f r)

instance Foldable Tree where
  foldr f z (Leaf a) = f a z
  foldr f z (Branch l r) = foldr f (foldr f z r) l

-- class hierarchy with superclass and defaults
class Shape a where
  area :: a -> Double
  perimeter :: a -> Double
  describe :: a -> String
  describe x = "shape with area " ++ show (area x)

class Shape a => Solid a where
  volume :: a -> Double -> Double
  volume x h = area x * h

data Square = Square Double
instance Shape Square where
  area (Square s) = s * s
  perimeter (Square s) = 4 * s
instance Solid Square

-- Monoid instance and mconcat
newtype MaxInt = MaxInt Int deriving Show
instance Semigroup MaxInt where
  MaxInt a <> MaxInt b = MaxInt (max a b)
instance Monoid MaxInt where
  mempty = MaxInt minBound

-- polymorphism with constraints
pairUp :: (Show a, Show b) => a -> b -> String
pairUp a b = "<" ++ show a ++ "|" ++ show b ++ ">"

average :: (Fractional a) => [a] -> a
average xs = sum xs / fromIntegral (length xs)

-- Maybe and Either monads
safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv a b = Just (a `div` b)

calc :: Int -> Int -> Int -> Maybe Int
calc a b c = do
  x <- safeDiv a b
  y <- safeDiv x c
  return (x + y)

parseAge :: String -> Either String Int
parseAge s = case reads s of
  [(n, "")] | n >= 0 -> Right n
            | otherwise -> Left "negative"
  _ -> Left ("bad number: " ++ s)

-- local polymorphic function used at two types
twice :: (Int, String)
twice = let dup x = (x, x) in (fst (dup 3), snd (dup "s"))

-- mutual recursion
isEven', isOdd' :: Int -> Bool
isEven' 0 = True
isEven' n = isOdd' (n - 1)
isOdd' 0 = False
isOdd' n = isEven' (n - 1)

main :: IO ()
main = do
  print (runState counter 5)
  print (evalState (labelTree (Branch (Leaf 'a') (Branch (Leaf 'b') (Leaf 'c')))) 0)
  let t = Branch (Leaf 3) (Branch (Leaf 4) (Leaf 5)) :: Tree Int
  print (fmap (* 2) t)
  print (sum t, product t, length t, maximum t, elem 4 t, foldr (:) [] t)
  putStrLn (describe (Square 3))
  print (volume (Square 2) 10, perimeter (Square 2))
  print (mconcat (map MaxInt [3, 9, 2]))
  putStrLn (pairUp (1 :: Int) "x")
  print (average [1, 2, 3, 4 :: Double])
  print (calc 100 5 2, calc 1 0 2)
  print (map parseAge ["42", "-1", "x"])
  print twice
  print (isEven' 10, isOdd' 7)
  print (sequence [Just 1, Just 2], sequence [Just 1, Nothing :: Maybe Int])
  print (traverse (\x -> if x > 0 then Right x else Left x) [1, 2, 3 :: Int])
  print (mapM (\x -> [x, x * 10]) [1, 2 :: Int])
  print (fmap length (Just "abc"), (+ 1) <$> Right 2 :: Either String Int)
  print (pure 5 :: [Int], (,) <$> Just 1 <*> Just "x")
  print (concat <$> mapM (\c -> [[c], [c, c]]) "ab")
  print (intercalate ", " ["a", "b", "c"])
  print ([1, 2] <> [3], "ab" <> "cd", mempty :: String)
  print (compare (1, 'b') (1, 'a'), (3, "x") < (3, "y"))
  print (foldMap (\x -> [x, x]) [1, 2, 3 :: Int])
