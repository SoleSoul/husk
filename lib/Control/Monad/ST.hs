-- Control.Monad.ST: strict state threads, implemented on top of IO.
-- runST's type is simplified to  ST s a -> a  (no rank-2 types in husk).
module Control.Monad.ST (ST, runST, stToIO) where

import Husk.Base

newtype ST s a = ST (IO a)

unST :: ST s a -> IO a
unST (ST io) = io

instance Functor (ST s) where
  fmap f (ST io) = ST (fmap f io)

instance Applicative (ST s) where
  pure x = ST (return x)
  ST f <*> ST x = ST (f <*> x)

instance Monad (ST s) where
  ST m >>= k = ST (m >>= \a -> unST (k a))

runST :: ST s a -> a
runST (ST io) = unsafePerformIO io

stToIO :: ST s a -> IO a
stToIO (ST io) = io
