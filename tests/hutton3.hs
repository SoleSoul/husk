-- Exercises from Graham Hutton, "Programming in Haskell" (2nd ed.),
-- chapters 10, 11, 13 and 17, with my own solutions.  Reads hutton3.in.
import Control.Applicative
import Data.Char
import Data.List (transpose)
import System.IO

-- Chapter 10: interactive programming
putStr' :: String -> IO ()
putStr' s = sequence_ [putChar c | c <- s]

type Board = [Int]

putRow :: Int -> Int -> IO ()
putRow row num = putStrLn (show row ++ ": " ++ concat (replicate num "* "))

putBoardR :: Board -> IO ()
putBoardR = go 1
  where go _ [] = return ()
        go r (n : ns) = putRow r n >> go (r + 1) ns

putBoardC :: Board -> IO ()
putBoardC b = sequence_ [putRow r n | (r, n) <- zip [1 ..] b]

adder :: IO ()
adder = do
  putStr "How many numbers? "
  n <- readLn
  xs <- sequence [readLn | _ <- [1 .. n :: Int]]
  putStrLn ("The total is " ++ show (sum xs :: Int))

-- read a line, where '\DEL' erases the previous character
readLine :: IO String
readLine = go []
  where
    go acc = do
      c <- getChar
      case c of
        '\n' -> return (reverse acc)
        '\DEL' -> go (drop 1 acc)
        _ -> go (c : acc)

-- nim, played from input
nimMove :: Board -> Int -> Int -> Board
nimMove board row num = [update r n | (r, n) <- zip [1 ..] board]
  where update r n = if r == row then n - num else n

nim :: Board -> Int -> IO ()
nim board player
  | all (== 0) board = putStrLn ("Player " ++ show (next player) ++ " wins!")
  | otherwise = do
      putBoardC board
      putStrLn ("Player " ++ show player)
      row <- readLn
      num <- readLn
      if board !! (row - 1) >= num
        then nim (nimMove board row num) (next player)
        else putStrLn "ERROR: Invalid move" >> nim board player
  where next 1 = 2
        next _ = 1

-- Chapter 11: tic-tac-toe game trees
data Player = O | B | X deriving (Eq, Ord, Show)

type Grid = [[Player]]

data Tree a = Node a [Tree a] deriving Show

emptyGrid :: Grid
emptyGrid = replicate 3 (replicate 3 B)

turn :: Grid -> Player
turn g = if os <= xs then O else X
  where os = length (filter (== O) ps)
        xs = length (filter (== X) ps)
        ps = concat g

wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
  where line = all (== p)
        rows = g
        cols = transpose g
        dias = [[g !! n !! n | n <- [0 .. 2]], [g !! n !! (2 - n) | n <- [0 .. 2]]]

won :: Grid -> Bool
won g = wins O g || wins X g

full :: Grid -> Bool
full = all (/= B) . concat

moves :: Grid -> Player -> [Grid]
moves g p
  | won g = []
  | full g = []
  | otherwise = [chop (xs ++ [p] ++ ys) | i <- [0 .. 8], concat g !! i == B, let (xs, _ : ys) = splitAt i (concat g)]
  where chop [] = []
        chop zs = take 3 zs : chop (drop 3 zs)

gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]
  where next O = X
        next X = O
        next B = B

nodes :: Tree a -> Int
nodes (Node _ ts) = 1 + sum (map nodes ts)

depth :: Tree a -> Int
depth (Node _ []) = 0
depth (Node _ ts) = 1 + maximum (map depth ts)

minimax :: Tree Grid -> Tree (Grid, Player)
minimax (Node g [])
  | wins O g = Node (g, O) []
  | wins X g = Node (g, X) []
  | otherwise = Node (g, B) []
minimax (Node g ts)
  | turn g == O = Node (g, minimum ps) ts'
  | otherwise = Node (g, maximum ps) ts'
  where ts' = map minimax ts
        ps = [p | Node (_, p) _ <- ts']

-- the best move that wins soonest (exercise 3)
bestmove :: Grid -> Player -> Grid
bestmove g p = snd (minimum [(depth t, g') | t@(Node (g', p') _) <- ts, p' == best])
  where Node (_, best) ts = minimax (gametree g p)

-- Chapter 13: monadic parsing
newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) = p

item :: Parser Char
item = P (\inp -> case inp of
                    [] -> []
                    (x : xs) -> [(x, xs)])

instance Functor Parser where
  fmap g p = P (\inp -> [(g v, out) | (v, out) <- parse p inp])

instance Applicative Parser where
  pure v = P (\inp -> [(v, inp)])
  pg <*> px = P (\inp -> [(g v, out') | (g, out) <- parse pg inp, (v, out') <- parse px out])

instance Monad Parser where
  p >>= f = P (\inp -> concat [parse (f v) out | (v, out) <- parse p inp])

instance Alternative Parser where
  empty = P (const [])
  p <|> q = P (\inp -> case parse p inp of
                         [] -> parse q inp
                         rs -> rs)

sat :: (Char -> Bool) -> Parser Char
sat p = do { x <- item; if p x then return x else empty }

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x : xs) = do { _ <- char x; _ <- string xs; return (x : xs) }

space :: Parser ()
space = do { _ <- many (sat isSpace); return () }

token :: Parser a -> Parser a
token p = do { space; v <- p; space; return v }

symbol :: String -> Parser String
symbol = token . string

natural :: Parser Int
natural = token (read <$> some (sat isDigit))

-- exercise 1
comment :: Parser ()
comment = do
  _ <- string "--"
  _ <- many (sat (/= '\n'))
  _ <- char '\n'
  return ()

-- exercises 5-7: expression trees with - / and ^
data Expr = Num Int | Bin Char Expr Expr deriving Show

expr :: Parser Expr
expr = do
  t <- term
  rest t
  where
    rest acc = (do { o <- token (sat (`elem` "+-")); t <- term; rest (Bin o acc t) }) <|> return acc

term :: Parser Expr
term = do
  f <- power
  rest f
  where
    rest acc = (do { o <- token (sat (`elem` "*/")); f <- power; rest (Bin o acc f) }) <|> return acc

power :: Parser Expr
power = do
  f <- factor
  do { _ <- symbol "^"; p <- power; return (Bin '^' f p) }
    <|> return f

factor :: Parser Expr
factor = do { _ <- symbol "("; e <- expr; _ <- symbol ")"; return e } <|> Num <$> natural

evalE :: Expr -> Int
evalE (Num n) = n
evalE (Bin o l r) = op o (evalE l) (evalE r)
  where op '+' = (+)
        op '-' = (-)
        op '*' = (*)
        op '/' = div
        op '^' = (^)

calc :: String -> String
calc s = case parse expr s of
  [(e, "")] -> show (evalE e)
  [(_, rest)] -> "unused input: " ++ rest
  _ -> "invalid input"

-- Chapter 17: calculating compilers, with exceptions
data CExpr = Val Int | Add CExpr CExpr | Throw | Catch CExpr CExpr

evalC :: CExpr -> Maybe Int
evalC (Val n) = Just n
evalC (Add x y) = (+) <$> evalC x <*> evalC y
evalC Throw = Nothing
evalC (Catch x h) = evalC x <|> evalC h

data Code = PUSH Int Code | ADD Code | FAIL | MARK Code Code | UNMARK Code | HALT deriving Show

data Elem = VAL Int | HAN Code

comp :: CExpr -> Code
comp e = comp' e HALT

comp' :: CExpr -> Code -> Code
comp' (Val n) c = PUSH n c
comp' (Add x y) c = comp' x (comp' y (ADD c))
comp' Throw _ = FAIL
comp' (Catch x h) c = MARK (comp' h c) (comp' x (UNMARK c))

exec :: Code -> [Elem] -> [Elem]
exec (PUSH n c) s = exec c (VAL n : s)
exec (ADD c) (VAL m : VAL n : s) = exec c (VAL (n + m) : s)
exec FAIL s = fail' s
exec (MARK h c) s = exec c (HAN h : s)
exec (UNMARK c) (VAL n : HAN _ : s) = exec c (VAL n : s)
exec HALT s = s

fail' :: [Elem] -> [Elem]
fail' [] = []
fail' (VAL _ : s) = fail' s
fail' (HAN h : s) = exec h s

runC :: CExpr -> Maybe Int
runC e = case exec (comp e) [] of
  [VAL n] -> Just n
  _ -> Nothing

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStr' "hello\n"
  putBoardR [5, 4, 3, 2, 1]
  putBoardC [1, 2]
  adder
  l <- readLine
  print l
  nim [2, 1] 1
  print (nodes (gametree emptyGrid O), depth (gametree emptyGrid O))
  print (bestmove [[O, B, B], [X, X, B], [B, B, B]] O, bestmove [[O, X, O], [B, X, B], [B, B, B]] O)
  print (parse comment "-- a comment\nrest", parse comment "- no")
  print (parse expr "2*3^2^1-4/2-1")
  mapM_ (putStrLn . calc) ["2*(3+4)", "10-2-3", "2^3^2", "100/7/2", "1+", "(1"]
  let e = Add (Val 1) (Catch (Add (Val 2) Throw) (Val 10))
  print (evalC e, runC e, comp e, runC (Add Throw (Val 1)))
