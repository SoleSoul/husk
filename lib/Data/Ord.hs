module Data.Ord (Ord(..), Ordering(..), comparing, Down(..), getDown, clamp) where
import Husk.Base

clamp :: Ord a => (a, a) -> a -> a
clamp (low, high) a = min high (max a low)
