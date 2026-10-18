{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- newtype deriving (GeneralizedNewtypeDeriving) and inferred contexts
import Control.Monad.State

newtype Size = Size Int deriving (Eq, Ord, Show, Num)

newtype Score = Score Double deriving (Eq, Ord, Num, Fractional, Real, RealFrac)

newtype Counter a = Counter (State Int a) deriving (Functor, Applicative, Monad)

newtype App m a = App (StateT [String] m a) deriving (Functor, Applicative, Monad)

newtype Wrap = Wrap [Int] deriving (Show, Semigroup, Monoid)

newtype Age = Age Int deriving (Show, Eq, Ord, Enum, Bounded, Integral, Real, Num)

-- contexts like (Show (f (g a))) are inferred
newtype Both f g a = Both (f (g a)) deriving (Show, Eq)
data T f a = T (Maybe (f a)) [a] deriving Show
data Rose a = Rose a [Rose a] deriving (Show, Eq, Ord)

tick :: Counter Int
tick = Counter (do { n <- get; put (n + 1); return n })

runCounter :: Counter a -> a
runCounter (Counter m) = evalState m 0

logMsg :: Monad m => String -> App m ()
logMsg s = App (modify (++ [s]))

runApp :: Monad m => App m a -> m (a, [String])
runApp (App m) = runStateT m []

main :: IO ()
main = do
  print (Size 3 + Size 4 * 2, sum (map Size [1 .. 10]), maximum [Size 2, Size 9])
  let Score s = Score 7 / 2 in print s
  print (truncate (Score 7.9) :: Int)
  print (runCounter (do { a <- tick; b <- tick; c <- fmap (* 10) tick; return [a, b, c] }))
  r <- runApp (logMsg "hello" >> logMsg "world" >> return 42)
  print r
  print (Wrap [1] <> Wrap [2, 3] <> mempty)
  print ([Age 1 .. Age 3], Age 7 `div` Age 2, toInteger (Age 5))
  print (Both (Just [True]), Both [Nothing :: Maybe Int] == Both [Nothing])
  print (T (Just [1, 2]) [3 :: Int])
  print (Rose 1 [Rose 2 []] < Rose 1 [Rose (3 :: Int) []])
