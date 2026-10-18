-- Exercises in the style of the Helsinki Haskell MOOC (my own solutions):
-- a NoImplicitPrelude module, the `todo` exception, State, phantom types
-- and an applicative Validation.
import Course.Recursion
import Course.Todo
import Control.Exception
import Control.Monad.Trans.State
import Control.Applicative
import Data.Char (isDigit)
import Data.List.NonEmpty (NonEmpty ((:|)))

-- State: occurrences of values
count :: Eq a => a -> State [(a, Int)] ()
count x = modify bump
  where bump [] = [(x, 1)]
        bump ((y, n) : rest) | y == x = (y, n + 1) : rest
                             | otherwise = (y, n) : bump rest

occurrences :: Eq a => [a] -> State [(a, Int)] Int
occurrences xs = mapM_ count xs >> gets length

-- phantom types
data EUR
data USD
newtype Money c = Money Double deriving Show
data Rate from to = Rate Double deriving Show

convert :: Rate a b -> Money a -> Money b
convert (Rate r) (Money x) = Money (r * x)

composeRates :: Rate a b -> Rate b c -> Rate a c
composeRates (Rate x) (Rate y) = Rate (x * y)

class Render c where
  render :: Money c -> String

instance Render EUR where
  render (Money x) = show x ++ "e"

instance Render USD where
  render (Money x) = "$" ++ show x

-- an applicative that collects all errors
data Validation a = Ok a | Errors [String] deriving Show

instance Functor Validation where
  fmap f (Ok x) = Ok (f x)
  fmap _ (Errors e) = Errors e

instance Applicative Validation where
  pure = Ok
  liftA2 f (Ok x) (Ok y) = Ok (f x y)
  liftA2 _ (Errors a) (Ok _) = Errors a
  liftA2 _ (Ok _) (Errors b) = Errors b
  liftA2 _ (Errors a) (Errors b) = Errors (a ++ b)

instance Alternative Validation where
  empty = Errors []
  Ok x <|> _ = Ok x
  Errors _ <|> Ok y = Ok y
  Errors a <|> Errors b = Errors (a ++ b)

check :: Bool -> String -> a -> Validation a
check b err x = if b then pure x else Errors [err]

data Address = Address String String String deriving Show

validateAddress :: String -> String -> String -> Validation Address
validateAddress street number post =
  Address <$> check (length street <= 20) "Invalid street name" street
          <*> check (all isDigit number) "Invalid street number" number
          <*> check (length post == 5 && all isDigit post) "Invalid postcode" post

average :: Fractional a => NonEmpty a -> a
average (x :| xs) = sum (x : xs) / fromIntegral (1 + length xs)

main :: IO ()
main = do
  print (sumsOf [1, 2, 3, 4], merge [1, 3, 5] [2, 4, 6], mymaximum (>) 0 [1, 3, 2])
  r <- try (evaluate (map2 (+) [1] [2 :: Int]))
  putStrLn (either (\e -> "not done: " ++ show (e :: TODO)) show r)
  print (runState (occurrences [5, 5, 6, 6, 5, 6, 7]) [])
  print (convert (Rate 1.25 :: Rate EUR USD) (Money 2), render (Money 1.5 :: Money EUR), render (convert (composeRates (Rate 2) (Rate 0.5 :: Rate USD EUR)) (Money 3 :: Money EUR)))
  print (validateAddress "Haskell road" "35" "13337")
  print (validateAddress "Haskeller's favourite road" "35a" "1333")
  print (check False "first" 1 <|> check False "second" (2 :: Int), traverse (\x -> check (x > 0) ("bad " ++ show x) x) [1, -2, 3, -4 :: Int])
  print (average (1.0 :| [2.0, 3.0]))
  -- an exception that nobody catches
  print (map2 (+) [1] [2 :: Int])
