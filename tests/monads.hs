import Text.Printf
import Control.Monad.State
import Control.Monad.ST
import Data.STRef

fibMemo :: Int -> State [(Int, Integer)] Integer
fibMemo n
  | n < 2 = return (fromIntegral n)
  | otherwise = do
      memo <- get
      case lookup n memo of
        Just v -> return v
        Nothing -> do
          a <- fibMemo (n - 1)
          b <- fibMemo (n - 2)
          let r = a + b
          modify ((n, r) :)
          return r

push :: Int -> State [Int] ()
push x = modify (x :)

pop :: State [Int] (Maybe Int)
pop = do
  s <- get
  case s of
    [] -> return Nothing
    (x:xs) -> put xs >> return (Just x)

stackOps :: State [Int] (Maybe Int, Maybe Int)
stackOps = do
  push 1
  push 2
  forM_ [3 .. 5] push
  a <- pop
  _ <- pop
  b <- pop
  return (a, b)

sumST :: [Int] -> Int
sumST xs = runST $ do
  ref <- newSTRef 0
  mapM_ (\x -> modifySTRef' ref (+ x)) xs
  readSTRef ref

main :: IO ()
main = do
  printf "%d %s %c\n" (42 :: Int) "str" 'z'
  printf "%5d|%-5d|%05d\n" (7 :: Int) (7 :: Int) (7 :: Int)
  printf "%.2f %8.3f %e\n" (3.14159 :: Double) (2.5 :: Double) (1234.5 :: Double)
  printf "%x %X %o %%\n" (255 :: Int) (255 :: Int) (8 :: Int)
  let s = printf "%d-%d" (1 :: Int) (2 :: Int) :: String
  putStrLn s
  printf "%s has %d items\n" "list" (length [1, 2, 3])
  print (evalState (fibMemo 80) [])
  print (runState stackOps [])
  print (execState (mapM_ (\x -> modify (+ x)) [1 .. 100]) 0)
  print (evalState (gets length) "abc")
  print (sumST [1 .. 1000])
  r <- foldM (\acc x -> do { print x; return (acc + x) }) 0 [1, 2, 3]
  print r
  print =<< mapM (return . (* 2)) [1, 2, 3]
  print (zipWith3 (\a b c -> a + b + c) [1, 2] [10, 20] [100, 200])
  print (replicateM 2 "ab")
  print (filterM (const [True, False]) [1, 2])
  print $ do { x <- [1, 2, 3]; y <- "ab"; guard (x /= 2); return (x, y) }
