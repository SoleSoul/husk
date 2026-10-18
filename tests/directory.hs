import System.Directory
import System.IO
import System.IO.Error
import System.Random (randomRIO)
import Control.Exception
import Data.List (sort)

main :: IO ()
main = do
  tmp <- getTemporaryDirectory
  n <- randomRIO (0, 10^9 :: Int)
  let dir = tmp ++ "/husk-test-" ++ show n
  createDirectory dir
  withCurrentDirectory dir run
  removeDirectoryRecursive dir
  doesDirectoryExist dir >>= print

run :: IO ()
run = do
  writeFile "todo.txt" "Iron the dishes\nDust the dog\nTake salad out of the oven\n"
  print =<< mapM doesFileExist ["todo.txt", "nope.txt", "."]
  print =<< doesDirectoryExist "."
  -- LYAH's todo "remove" command
  handle <- openFile "todo.txt" ReadMode
  contents <- hGetContents handle
  let newItems = unlines (filter (/= "Dust the dog") (lines contents))
  bracketOnError (openTempFile "." "temp")
    (\(tempName, tempHandle) -> hClose tempHandle >> removeFile tempName)
    (\(tempName, tempHandle) -> do
        hPutStr tempHandle newItems
        hClose handle
        hClose tempHandle
        removeFile "todo.txt"
        renameFile tempName "todo.txt")
  readFile "todo.txt" >>= putStr
  copyFile "todo.txt" "copy.txt"
  createDirectoryIfMissing True "a/b/c"
  createDirectoryIfMissing False "a"
  writeFile "a/b/c/f" "x"
  print =<< fmap sort (listDirectory ".")
  print =<< getDirectoryContents "a"
  print =<< getFileSize "copy.txt"
  -- IO errors
  r <- tryIOError (removeFile "nope.txt")
  case r of
    Left e -> print (isDoesNotExistError e, ioeGetFileName e, ioeGetErrorString e, show e)
    Right _ -> putStrLn "no error"
  r2 <- tryIOError (createDirectory "a")
  print (either isAlreadyExistsError (const False) r2)
  readFile "nope.txt" `catchIOError` (\e -> return ("caught: " ++ show e)) >>= putStrLn
  r3 <- tryIOError (ioError (userError "custom"))
  print (either (\e -> (isUserError e, ioeGetErrorString e)) (const (False, "")) r3)
  -- catchIOError lets other exceptions through
  r4 <- try (catchIOError (evaluate (error "not an IO error" :: Int)) (\_ -> return 0))
  putStrLn (either (\e -> "passed through: " ++ show (e :: SomeException)) show r4)
  r5 <- try (bracket_ (putStrLn "acquire") (putStrLn "release") (ioError (userError "inside")))
  putStrLn (either (\e -> show (e :: SomeException)) (const "") r5)
  (onException (return 1) (putStrLn "not run") :: IO Int) >>= print
