-- Control.Arrow: the arrow combinators, for functions.  (Only the function
-- arrow (->) is an instance; Kleisli arrows are not provided.)
module Control.Arrow (
  Arrow(..), ArrowChoice(..), (>>>), (<<<), returnA
  ) where
import Husk.Base

infixr 5 +++
infixr 3 ***
infixr 3 &&&
infixr 2 |||
infixr 1 >>>, <<<

class Arrow a where
  arr :: (b -> c) -> a b c
  first :: a b c -> a (b, d) (c, d)
  second :: a b c -> a (d, b) (d, c)
  (***) :: a b c -> a b' c' -> a (b, b') (c, c')
  (&&&) :: a b c -> a b c' -> a b (c, c')

instance Arrow (->) where
  arr f = f
  first f (x, y) = (f x, y)
  second f (x, y) = (x, f y)
  (f *** g) (x, y) = (f x, g y)
  (f &&& g) x = (f x, g x)

class Arrow a => ArrowChoice a where
  left :: a b c -> a (Either b d) (Either c d)
  right :: a b c -> a (Either d b) (Either d c)
  (+++) :: a b c -> a b' c' -> a (Either b b') (Either c c')
  (|||) :: a b d -> a c d -> a (Either b c) d

instance ArrowChoice (->) where
  left f = either (Left . f) Right
  right f = either Left (Right . f)
  f +++ g = either (Left . f) (Right . g)
  (|||) = either

(>>>) :: (a -> b) -> (b -> c) -> a -> c
f >>> g = g . f

(<<<) :: (b -> c) -> (a -> b) -> a -> c
(<<<) = (.)

returnA :: a -> a
returnA x = x
