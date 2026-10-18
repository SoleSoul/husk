-- Control.Monad.Random: random numbers in a monad (the MonadRandom package).
--   Rand g a  is a computation that draws from a generator g;
--   run it with evalRand, runRand or evalRandIO.
-- IO is an instance too, using the global generator.
module Control.Monad.Random (
  RandomGen(..), StdGen, mkStdGen, Random(..), getStdGen, setStdGen, newStdGen,
  getStdRandom, randomIO, randomRIO,
  module Control.Monad, MonadTrans(..), MonadIO(..),
  MonadRandom(..), MonadSplit(..), RandT, Rand, liftRand, liftRandT,
  runRand, evalRand, execRand, runRandT, evalRandT, execRandT, evalRandIO,
  fromList, weighted, uniform
  ) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.Functor.Identity
import System.Random hiding (uniform, uniformR)

class Monad m => MonadRandom m where
  getRandom :: Random a => m a
  getRandomR :: Random a => (a, a) -> m a
  getRandoms :: Random a => m [a]
  getRandomRs :: Random a => (a, a) -> m [a]

class Monad m => MonadSplit m where
  getSplit :: m StdGen

newtype RandT g m a = RandT (StateT g m a) deriving (Functor, Applicative, Monad, MonadIO)

type Rand g = RandT g Identity

instance MonadTrans (RandT g) where
  lift m = RandT (lift m)

liftRandT :: (g -> m (a, g)) -> RandT g m a
liftRandT f = RandT (StateT f)

liftRand :: Monad m => (g -> (a, g)) -> RandT g m a
liftRand f = RandT (state f)

instance (RandomGen g, Monad m) => MonadRandom (RandT g m) where
  getRandom = liftRand random
  getRandomR r = liftRand (randomR r)
  getRandoms = liftRand (\g -> let (a, b) = split g in (randoms a, b))
  getRandomRs r = liftRand (\g -> let (a, b) = split g in (randomRs r a, b))

instance Monad m => MonadSplit (RandT StdGen m) where
  getSplit = liftRand split

instance MonadRandom IO where
  getRandom = randomIO
  getRandomR = randomRIO
  getRandoms = fmap randoms newStdGen
  getRandomRs r = fmap (randomRs r) newStdGen

instance MonadSplit IO where
  getSplit = newStdGen

runRandT :: RandT g m a -> g -> m (a, g)
runRandT (RandT m) = runStateT m

evalRandT :: Monad m => RandT g m a -> g -> m a
evalRandT m g = fmap fst (runRandT m g)

execRandT :: Monad m => RandT g m a -> g -> m g
execRandT m g = fmap snd (runRandT m g)

runRand :: Rand g a -> g -> (a, g)
runRand m g = runIdentity (runRandT m g)

evalRand :: Rand g a -> g -> a
evalRand m g = fst (runRand m g)

execRand :: Rand g a -> g -> g
execRand m g = snd (runRand m g)

evalRandIO :: Rand StdGen a -> IO a
evalRandIO m = getStdRandom (runRand m)

-- a value picked with the given weights
fromList :: MonadRandom m => [(a, Rational)] -> m a
fromList [] = error "Control.Monad.Random.fromList: empty list"
fromList xs = do
  let total = sum (map snd xs)
  r <- getRandomR (0, fromRational total :: Double)
  let pick ((x, _) : []) _ = x
      pick ((x, w) : rest) acc = if r < acc + fromRational w then x else pick rest (acc + fromRational w)
  return (pick xs 0)

weighted :: MonadRandom m => [(a, Rational)] -> m a
weighted = fromList

uniform :: MonadRandom m => [a] -> m a
uniform xs = fromList [(x, 1) | x <- xs]
