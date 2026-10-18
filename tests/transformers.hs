import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer

-- StateT over IO: a counter that logs as it goes
counter :: StateT Int IO ()
counter = do
  forM_ [1 .. 3] $ \i -> do
    modify (+ i)
    s <- get
    liftIO (putStrLn ("after " ++ show i ++ ": " ++ show s))
  n <- get
  when (n > 5) $ lift (putStrLn "big total")

data Config = Config { verbose :: Bool, name :: String }

greeting :: Reader Config String
greeting = do
  n <- asks name
  v <- asks verbose
  return (if v then "Hello, " ++ n ++ "! Welcome." else "Hi " ++ n)

collatz :: Int -> Writer [Int] Int
collatz 1 = do
  tell [1]
  return 0
collatz n = do
  tell [n]
  steps <- collatz (if even n then n `div` 2 else 3 * n + 1)
  return (steps + 1)

-- a simple stack machine with State
data Instr = Push Int | Add | Mul | Dup deriving Show

run :: [Instr] -> State [Int] ()
run = mapM_ exec
  where
    exec (Push n) = modify (n :)
    exec Add = binop (+)
    exec Mul = binop (*)
    exec Dup = modify (\(x:xs) -> x : x : xs)
    binop f = modify (\(x:y:rest) -> f y x : rest)

main :: IO ()
main = do
  ((), total) <- runStateT counter 0
  print total
  putStrLn (runReader greeting (Config True "Ada"))
  putStrLn (runReader (local (\c -> c { verbose = False }) greeting) (Config True "Bob"))
  print (runWriter (collatz 6))
  print (execState (run [Push 2, Push 3, Add, Dup, Mul]) [])
  n <- execStateT (mapM_ (\x -> modify (+ x)) [1 .. 100]) 0
  print n
  print (evalState (traverse (\x -> state (\s -> (x + s, s + 1))) [10, 20, 30]) 0)
