-- An interactive calculator: a tokenizer, a recursive-descent parser for
-- arithmetic with variables, and an evaluator.  Try:
--   x = 3 * (2 + 4)
--   x / 4 - 1
import qualified Data.Map as Map
import Data.Char (isDigit, isSpace, isAlpha)
import System.IO

data Token = TNum Double | TIdent String | TOp Char | TLParen | TRParen
  deriving (Show, Eq)

data Expr = Num Double | Var String | BinOp Char Expr Expr | Neg Expr
  deriving Show

tokenize :: String -> Either String [Token]
tokenize [] = Right []
tokenize (c:cs)
  | isSpace c = tokenize cs
  | isDigit c = let (ds, rest) = span (\x -> isDigit x || x == '.') (c:cs)
                in (TNum (read ds) :) <$> tokenize rest
  | isAlpha c = let (name, rest) = span isAlpha (c:cs)
                in (TIdent name :) <$> tokenize rest
  | c `elem` "+-*/^=" = (TOp c :) <$> tokenize cs
  | c == '(' = (TLParen :) <$> tokenize cs
  | c == ')' = (TRParen :) <$> tokenize cs
  | otherwise = Left ("unexpected character " ++ show c)

-- expr := term (('+'|'-') term)* ; term := factor (('*'|'/') factor)*
-- factor := unary ('^' factor)? ; unary := '-' unary | atom
type Parser a = [Token] -> Either String (a, [Token])

expr :: Parser Expr
expr ts = term ts >>= uncurry (loop "+-" term)

term :: Parser Expr
term ts = factor ts >>= uncurry (loop "*/" factor)

loop :: String -> Parser Expr -> Expr -> [Token] -> Either String (Expr, [Token])
loop ops next lhs (TOp o : rest)
  | o `elem` ops = do
      (rhs, rest') <- next rest
      loop ops next (BinOp o lhs rhs) rest'
loop _ _ lhs ts = Right (lhs, ts)

factor :: Parser Expr
factor ts = do
  (base, rest) <- unary ts
  case rest of
    TOp '^' : rest' -> do
      (ex, rest'') <- factor rest'
      return (BinOp '^' base ex, rest'')
    _ -> return (base, rest)

unary :: Parser Expr
unary (TOp '-' : rest) = do
  (e, rest') <- unary rest
  return (Neg e, rest')
unary ts = atom ts

atom :: Parser Expr
atom (TNum n : rest) = Right (Num n, rest)
atom (TIdent v : rest) = Right (Var v, rest)
atom (TLParen : rest) = do
  (e, rest') <- expr rest
  case rest' of
    TRParen : rest'' -> Right (e, rest'')
    _ -> Left "expected )"
atom (t : _) = Left ("unexpected " ++ show t)
atom [] = Left "unexpected end of input"

eval :: Map.Map String Double -> Expr -> Either String Double
eval _ (Num n) = Right n
eval env (Var v) = maybe (Left ("unknown variable " ++ v)) Right (Map.lookup v env)
eval env (Neg e) = negate <$> eval env e
eval env (BinOp op a b) = do
  x <- eval env a
  y <- eval env b
  case op of
    '+' -> Right (x + y)
    '-' -> Right (x - y)
    '*' -> Right (x * y)
    '/' | y == 0 -> Left "division by zero"
        | otherwise -> Right (x / y)
    '^' -> Right (x ** y)
    _ -> Left ("bad operator " ++ [op])

-- a line is either "name = expr" or "expr"
runLine :: Map.Map String Double -> String -> (String, Map.Map String Double)
runLine env line = case tokenize line of
  Left err -> ("error: " ++ err, env)
  Right (TIdent v : TOp '=' : rest) -> case parseAll rest >>= eval env of
    Left err -> ("error: " ++ err, env)
    Right x -> (v ++ " = " ++ show x, Map.insert v x env)
  Right ts -> case parseAll ts >>= eval env of
    Left err -> ("error: " ++ err, env)
    Right x -> (show x, env)
  where
    parseAll ts = do
      (e, rest) <- expr ts
      if null rest then Right e else Left ("unexpected " ++ show (head rest))

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "calculator -- enter expressions, or name = expr; Ctrl-D to quit"
  repl (Map.fromList [("pi", pi), ("e", exp 1)])
  where
    repl env = do
      putStr "> "
      eof <- isEOF
      if eof
        then putStrLn ""
        else do
          line <- getLine
          let (out, env') = runLine env line
          putStrLn out
          repl env'
