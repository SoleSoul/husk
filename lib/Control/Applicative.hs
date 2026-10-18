module Control.Applicative (
  Applicative(..), Alternative(..), (<$>), (<$), (<**>), liftA, liftA3, optional, asum,
  ZipList(..)
  ) where
import Husk.Base

optional :: Alternative f => f a -> f (Maybe a)
optional v = (Just <$> v) <|> pure Nothing

-- Lists with a zipping Applicative: fs <*> xs applies each function to the
-- element in the same position.
newtype ZipList a = ZipList { getZipList :: [a] } deriving (Eq, Ord, Show)

instance Functor ZipList where
  fmap f (ZipList xs) = ZipList (map f xs)

instance Applicative ZipList where
  pure x = ZipList (repeat x)
  ZipList fs <*> ZipList xs = ZipList (zipWith id fs xs)

instance Foldable ZipList where
  foldr f z (ZipList xs) = foldr f z xs
  toList (ZipList xs) = xs

instance Traversable ZipList where
  traverse f (ZipList xs) = ZipList <$> traverse f xs
