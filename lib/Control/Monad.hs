module Control.Monad (
  Functor(..), Monad(..), MonadFail(..), MonadPlus(..),
  mapM, mapM_, forM, forM_, sequence, sequence_, (=<<), (>=>), (<=<), forever, void,
  join, msum, mplus, mzero, mfilter, filterM, mapAndUnzipM, zipWithM, zipWithM_,
  foldM, foldM_, replicateM, replicateM_, guard, when, unless,
  liftM, liftM2, ap, (<$!>), (<$>)
  ) where
import Husk.Base

mfilter :: MonadPlus m => (a -> Bool) -> m a -> m a
mfilter p ma = ma >>= \a -> if p a then return a else mzero

(<$!>) :: Monad m => (a -> b) -> m a -> m b
f <$!> m = m >>= \x -> let z = f x in z `seq` return z
infixl 4 <$!>
