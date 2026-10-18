-- Control.Monad.Trans.State: the state monad transformer
module Control.Monad.Trans.State (
  StateT(..), State, runState, evalState, execState, evalStateT, execStateT,
  state, get, put, modify, modify', gets, withState, mapState
  ) where

import Data.Functor.Identity
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

type State s = StateT s Identity

instance Monad m => Functor (StateT s m) where
  fmap f (StateT g) = StateT (\s -> g s >>= \(a, s') -> return (f a, s'))

instance Monad m => Applicative (StateT s m) where
  pure a = StateT (\s -> return (a, s))
  StateT mf <*> StateT mx = StateT (\s -> mf s >>= \(f, s1) -> mx s1 >>= \(x, s2) -> return (f x, s2))

instance Monad m => Monad (StateT s m) where
  StateT m >>= k = StateT (\s -> m s >>= \(a, s') -> runStateT (k a) s')

instance MonadFail m => MonadFail (StateT s m) where
  fail msg = StateT (\_ -> fail msg)

instance MonadTrans (StateT s) where
  lift m = StateT (\s -> m >>= \a -> return (a, s))

instance MonadIO m => MonadIO (StateT s m) where
  liftIO io = lift (liftIO io)

runState :: State s a -> s -> (a, s)
runState m s = runIdentity (runStateT m s)

evalState :: State s a -> s -> a
evalState m s = fst (runState m s)

execState :: State s a -> s -> s
execState m s = snd (runState m s)

evalStateT :: Monad m => StateT s m a -> s -> m a
evalStateT m s = runStateT m s >>= \(a, _) -> return a

execStateT :: Monad m => StateT s m a -> s -> m s
execStateT m s = runStateT m s >>= \(_, s') -> return s'

state :: Monad m => (s -> (a, s)) -> StateT s m a
state f = StateT (\s -> return (f s))

get :: Monad m => StateT s m s
get = StateT (\s -> return (s, s))

put :: Monad m => s -> StateT s m ()
put s = StateT (\_ -> return ((), s))

modify :: Monad m => (s -> s) -> StateT s m ()
modify f = StateT (\s -> return ((), f s))

modify' :: Monad m => (s -> s) -> StateT s m ()
modify' f = StateT (\s -> let s' = f s in s' `seq` return ((), s'))

gets :: Monad m => (s -> a) -> StateT s m a
gets f = StateT (\s -> return (f s, s))

withState :: Monad m => (s -> s) -> StateT s m a -> StateT s m a
withState f m = modify f >> m

mapState :: ((a, s) -> (b, s)) -> State s a -> State s b
mapState f m = StateT (\s -> Identity (f (runState m s)))
