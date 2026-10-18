-- Control.Monad.Trans.Reader: the reader monad transformer
module Control.Monad.Trans.Reader (
  ReaderT(..), Reader, runReader, reader, ask, asks, local, withReaderT
  ) where

import Data.Functor.Identity
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

type Reader r = ReaderT r Identity

instance Monad m => Functor (ReaderT r m) where
  fmap f (ReaderT g) = ReaderT (\r -> fmap f (g r))

instance Monad m => Applicative (ReaderT r m) where
  pure a = ReaderT (\_ -> return a)
  ReaderT mf <*> ReaderT mx = ReaderT (\r -> mf r >>= \f -> mx r >>= \x -> return (f x))

instance Monad m => Monad (ReaderT r m) where
  ReaderT m >>= k = ReaderT (\r -> m r >>= \a -> runReaderT (k a) r)

instance MonadTrans (ReaderT r) where
  lift m = ReaderT (\_ -> m)

instance MonadIO m => MonadIO (ReaderT r m) where
  liftIO io = lift (liftIO io)

runReader :: Reader r a -> r -> a
runReader m r = runIdentity (runReaderT m r)

reader :: Monad m => (r -> a) -> ReaderT r m a
reader f = ReaderT (\r -> return (f r))

ask :: Monad m => ReaderT r m r
ask = ReaderT return

asks :: Monad m => (r -> a) -> ReaderT r m a
asks f = ReaderT (\r -> return (f r))

local :: (r -> r) -> ReaderT r m a -> ReaderT r m a
local f (ReaderT g) = ReaderT (\r -> g (f r))

withReaderT :: (r' -> r) -> ReaderT r m a -> ReaderT r' m a
withReaderT f (ReaderT g) = ReaderT (\r -> g (f r))
