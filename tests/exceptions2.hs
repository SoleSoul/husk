{-# LANGUAGE DeriveAnyClass #-}
import Control.Exception
import System.IO.Error
import qualified Data.Map as M

data BankError = InsufficientFunds Int | UnknownAccount String deriving Show
instance Exception BankError

-- DeriveAnyClass style
data Other = Other deriving (Show, Exception)

withdraw :: Int -> Int -> IO Int
withdraw bal n
  | n > bal = throwIO (InsufficientFunds (n - bal))
  | otherwise = return (bal - n)

main :: IO ()
main = do
  r <- try (withdraw 10 25)
  case r of
    Left (InsufficientFunds k) -> putStrLn ("short by " ++ show k)
    Left e -> print e
    Right b -> print b
  r2 <- try (withdraw 10 5) :: IO (Either BankError Int)
  print r2
  -- a handler for one type lets others through
  r3 <- try (throwIO Other `catch` \e -> putStrLn ("bank: " ++ show (e :: BankError)))
  print (r3 :: Either Other ())
  -- pure throw, caught as SomeException
  r4 <- try (evaluate (throw (UnknownAccount "bob") :: Int))
  case r4 of
    Left e -> putStrLn ("caught: " ++ show (e :: SomeException))
    Right _ -> return ()
  -- fromException
  print (fromException (toException (UnknownAccount "x")) :: Maybe BankError)
  print (fromException (toException Other) :: Maybe BankError)
  -- error calls and arithmetic
  r5 <- try (evaluate (1 `div` (0 :: Int)))
  case r5 of
    Left DivideByZero -> putStrLn "divide by zero!"
    Left e -> print e
    Right v -> print v
  r6 <- try (evaluate (head ([] :: [Int])))
  case r6 of
    Left (ErrorCall m) -> putStrLn "ErrorCall"
    Right v -> print v
  handle (\e -> putStrLn ("io: " ++ show (isDoesNotExistError e))) (readFile "/nonexistent/x" >>= putStr)
  r7 <- try (ioError (userError "boom"))
  case r7 of
    Left e -> putStrLn (ioeGetErrorString e) >> print (isUserError e)
    Right () -> return ()
  -- an ErrorCall is not caught by an IOException handler
  r8 <- try (catchIOError (evaluate (error "pure" :: Int)) (\_ -> return 0))
  print (either (\(ErrorCall m) -> m) show r8)
  -- catchJust picks the exceptions it wants
  r9 <- try (catchJust (\e -> if e == DivideByZero then Just () else Nothing)
                       (evaluate (10 `mod` (0 :: Int)))
                       (\_ -> return (-1)))
  print (r9 :: Either ArithException Int)
  r10 <- tryJust (\(ErrorCall m) -> if m == "keep" then Just m else Nothing) (evaluate (error "keep" :: Int))
  print r10
  handle (\e -> putStrLn ("finally saw: " ++ show (e :: BankError)))
    (throwIO (InsufficientFunds 1) `finally` putStrLn "cleanup")
  throwIO (UnknownAccount "alice")
