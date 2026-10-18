module Main where
import Control.Monad (forM_, when)

process :: [Int] -> IO Int
process xs = do
  let total = sum xs
      count = length xs
  if total > 10
    then putStrLn "big"
    else putStrLn "small"
  forM_ xs $ \x ->
    when (even x) $ do
      let y = x * 2
      putStrLn ("even: " ++ show y)
  result <- case xs of
    [] -> return 0
    (y:_)
      | y > 2 -> do
          putStrLn "first > 2"
          return y
      | otherwise ->
          return (negate y)
  let go 0 acc = acc
      go n acc =
        go (n - 1) (acc + n)
  return (result + go count 0)

classify :: Int -> String
classify n = case compare n 0 of
  LT -> "negative"
  EQ -> "zero"
  GT | n > 100 -> "huge"
     | otherwise -> describe n
  where
    describe k
      | even k = "even positive"
      | otherwise = "odd positive"

data Config = Config
  { verbose :: Bool
  , level   :: Int
  , name    :: String
  } deriving (Show)

defaultConfig :: Config
defaultConfig = Config
  { verbose = False
  , level = 1
  , name = "default"
  }

longExpr :: Int
longExpr =
  let a = 1
      b = 2
  in a
     + b
     * 3

ifChain :: Int -> String
ifChain x =
  if x < 0 then "neg" else
  if x == 0 then "zero" else
  "pos"

main :: IO ()
main = do
  r <- process [1, 2, 3, 4]
  print r
  mapM_ (putStrLn . classify) [-1, 0, 7, 8, 1000]
  print defaultConfig { level = 3 }
  print longExpr
  putStrLn $ unwords $ map ifChain [-5, 0, 5]
  let nested = do
        x <- [1, 2]
        y <- [10, 20]
        return (x + y)
  print nested
  where
    unusedHelper = ()
