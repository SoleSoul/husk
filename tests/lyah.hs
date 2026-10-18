import Data.List (foldl')
import Control.Monad
import Data.Ratio

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
  Red == Red = True
  Green == Green = True
  Yellow == Yellow = True
  _ == _ = False

instance Show TrafficLight where
  show Red = "Red light"
  show Yellow = "Yellow light"
  show Green = "Green light"

class YesNo a where
  yesno :: a -> Bool

instance YesNo Int where
  yesno 0 = False
  yesno _ = True

instance YesNo [a] where
  yesno [] = False
  yesno _ = True

instance YesNo Bool where
  yesno = id

instance YesNo (Maybe a) where
  yesno (Just _) = True
  yesno Nothing = False

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult

-- RPN calculator with foldM
solveRPN :: String -> Maybe Double
solveRPN st = do
  [result] <- foldM foldingFunction [] (words st)
  return result

foldingFunction :: [Double] -> String -> Maybe [Double]
foldingFunction (x:y:ys) "*" = return ((x * y) : ys)
foldingFunction (x:y:ys) "+" = return ((x + y) : ys)
foldingFunction (x:y:ys) "-" = return ((y - x) : ys)
foldingFunction xs numberString = liftM (: xs) (readMaybe' numberString)

readMaybe' :: (Read a) => String -> Maybe a
readMaybe' st = case reads st of
  [(x, "")] -> Just x
  _ -> Nothing

-- knight's quest
type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c, r) = do
  (c', r') <- [(c + 2, r - 1), (c + 2, r + 1), (c - 2, r - 1), (c - 2, r + 1),
               (c + 1, r - 2), (c + 1, r + 2), (c - 1, r - 2), (c - 1, r + 2)]
  guard (c' `elem` [1 .. 8] && r' `elem` [1 .. 8])
  return (c', r')

in3 :: KnightPos -> [KnightPos]
in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start

-- a Writer monad
newtype Writer w a = Writer { runWriter :: (a, w) }

instance Functor (Writer w) where
  fmap f (Writer (a, w)) = Writer (f a, w)
instance Monoid w => Applicative (Writer w) where
  pure x = Writer (x, mempty)
  Writer (f, w1) <*> Writer (a, w2) = Writer (f a, w1 <> w2)
instance Monoid w => Monad (Writer w) where
  Writer (x, v) >>= f = let Writer (y, v') = f x in Writer (y, v <> v')

tell :: w -> Writer w ()
tell w = Writer ((), w)

gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
  | b == 0 = do
      tell ["Finished with " ++ show a]
      return a
  | otherwise = do
      tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
      gcd' b (a `mod` b)

-- probability monad
newtype Prob a = Prob { getProb :: [(a, Rational)] } deriving Show

instance Functor Prob where
  fmap f (Prob xs) = Prob $ map (\(x, p) -> (f x, p)) xs

flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob $ concat $ map multAll xs
  where multAll (Prob innerxs, p) = map (\(x, r) -> (x, p * r)) innerxs

instance Applicative Prob where
  pure x = Prob [(x, 1 % 1)]
  pf <*> px = flatten (fmap (\f -> fmap f px) pf)

instance Monad Prob where
  m >>= f = flatten (fmap f m)

data Coin = Heads | Tails deriving (Show, Eq)

coin :: Prob Coin
coin = Prob [(Heads, 1 % 2), (Tails, 1 % 2)]

loadedCoin :: Prob Coin
loadedCoin = Prob [(Heads, 1 % 10), (Tails, 9 % 10)]

flipThree :: Prob Bool
flipThree = do
  a <- coin
  b <- coin
  c <- loadedCoin
  return (all (== Tails) [a, b, c])

main :: IO ()
main = do
  print (Red == Red, Red == Yellow, [Red, Yellow, Green])
  print (yesno (length []), yesno "haha", yesno (Just 0), yesnoIf [] "YEAH!" "NO!")
  print (solveRPN "1 2 * 4 +", solveRPN "1 2 * 4 + 5 *", solveRPN "1 2 * 4", solveRPN "1 8 wharglbllargh")
  print (canReachIn3 (6, 2) (6, 1), canReachIn3 (6, 2) (7, 3))
  mapM_ putStrLn $ snd $ runWriter (gcd' 8 3)
  print (getProb flipThree)
