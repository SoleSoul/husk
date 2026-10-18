module Control.Applicative (
  Applicative(..), Alternative(..), (<$>), (<$), (<**>), liftA, liftA3, optional, asum
  ) where
import Husk.Base

optional :: Alternative f => f a -> f (Maybe a)
optional v = (Just <$> v) <|> pure Nothing
