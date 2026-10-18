module Data.Foldable (
  Foldable(..), foldrM, foldlM, traverse_, for_, sequenceA_, asum, mapM_, forM_,
  sequence_, msum, concat, concatMap, and, or, any, all, maximumBy, minimumBy,
  notElem, find, fold
  ) where
import Husk.Base

foldrM :: (Foldable t, Monad m) => (a -> b -> m b) -> b -> t a -> m b
foldrM f z0 xs = go (reverse (toList xs)) z0
  where go [] z = return z
        go (y:ys) z = f y z >>= go ys

foldlM :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
foldlM = foldM
