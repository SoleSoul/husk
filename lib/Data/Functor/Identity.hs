module Data.Functor.Identity (Identity(..)) where
import Husk.Base

newtype Identity a = Identity { runIdentity :: a } deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity (f a)

instance Monad Identity where
  Identity a >>= k = k a

instance Foldable Identity where
  foldr f z (Identity a) = f a z
  toList (Identity a) = [a]

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a
