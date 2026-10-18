import System.IO
import Control.Monad
import Data.IORef
import Text.Read (readMaybe)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  name <- getLine
  putStrLn ("Hello, " ++ name ++ "!")
  n <- readLn :: IO Int
  nums <- replicateM n (fmap read getLine) :: IO [Int]
  print (sum nums)
  line <- getLine
  let ws = map read (words line) :: [Integer]
  print (product ws)
  ref <- newIORef (0 :: Int)
  forM_ [1 .. 10] $ \i -> modifyIORef ref (+ i)
  readIORef ref >>= print
  when (n > 2) $ putStrLn "more than two"
  unless (n > 2) $ putStrLn "two or fewer"
  print (readMaybe "12" :: Maybe Int, readMaybe "x" :: Maybe Int)
  rest <- getContents
  let ls = lines rest
  mapM_ (putStrLn . reverse) ls
  print (length ls)
  eof <- isEOF
  print eof
