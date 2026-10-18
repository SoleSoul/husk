-- Control.Monad.Trans.Writer: the writer monad transformer
module Control.Monad.Trans.Writer (
  WriterT(..), Writer, runWriter, execWriter, execWriterT, writer, tell, listen, censor
  ) where

import Data.Functor.Identity
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }

type Writer w = WriterT w Identity

instance (Monoid w, Monad m) => Functor (WriterT w m) where
  fmap f (WriterT m) = WriterT (m >>= \(a, w) -> return (f a, w))

instance (Monoid w, Monad m) => Applicative (WriterT w m) where
  pure a = WriterT (return (a, mempty))
  WriterT mf <*> WriterT mx = WriterT (mf >>= \(f, w1) -> mx >>= \(x, w2) -> return (f x, w1 <> w2))

instance (Monoid w, Monad m) => Monad (WriterT w m) where
  WriterT m >>= k = WriterT (m >>= \(a, w1) -> runWriterT (k a) >>= \(b, w2) -> return (b, w1 <> w2))

instance Monoid w => MonadTrans (WriterT w) where
  lift m = WriterT (m >>= \a -> return (a, mempty))

instance (Monoid w, MonadIO m) => MonadIO (WriterT w m) where
  liftIO io = lift (liftIO io)

runWriter :: Writer w a -> (a, w)
runWriter m = runIdentity (runWriterT m)

execWriter :: Writer w a -> w
execWriter m = snd (runWriter m)

execWriterT :: Monad m => WriterT w m a -> m w
execWriterT m = runWriterT m >>= \(_, w) -> return w

writer :: Monad m => (a, w) -> WriterT w m a
writer aw = WriterT (return aw)

tell :: Monad m => w -> WriterT w m ()
tell w = WriterT (return ((), w))

listen :: Monad m => WriterT w m a -> WriterT w m (a, w)
listen (WriterT m) = WriterT (m >>= \(a, w) -> return ((a, w), w))

censor :: Monad m => (w -> w) -> WriterT w m a -> WriterT w m a
censor f (WriterT m) = WriterT (m >>= \(a, w) -> return (a, f w))
