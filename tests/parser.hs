import Control.Applicative
import Data.Char

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s -> case p s of
    Nothing -> Nothing
    Just (a, rest) -> Just (f a, rest)

instance Applicative Parser where
  pure x = Parser $ \s -> Just (x, s)
  Parser pf <*> Parser pa = Parser $ \s -> case pf s of
    Nothing -> Nothing
    Just (f, rest) -> case pa rest of
      Nothing -> Nothing
      Just (a, rest') -> Just (f a, rest')

instance Monad Parser where
  Parser p >>= f = Parser $ \s -> case p s of
    Nothing -> Nothing
    Just (a, rest) -> runParser (f a) rest

instance Alternative Parser where
  empty = Parser (const Nothing)
  Parser p <|> Parser q = Parser $ \s -> case p s of
    Nothing -> q s
    r -> r

satisfy :: (Char -> Bool) -> Parser Char
satisfy pr = Parser f
  where f (c:cs) | pr c = Just (c, cs)
        f _ = Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

spaces :: Parser ()
spaces = () <$ many (satisfy isSpace)

token :: Parser a -> Parser a
token p = p <* spaces

symbol :: Char -> Parser Char
symbol = token . char

number :: Parser Int
number = token (read <$> some (satisfy isDigit))

-- expr := term (('+' | '-') term)*
expr :: Parser Int
expr = do
  t <- term
  rest t
  where
    rest acc = (do _ <- symbol '+'; t <- term; rest (acc + t))
           <|> (do _ <- symbol '-'; t <- term; rest (acc - t))
           <|> pure acc

term :: Parser Int
term = do
  f <- factor
  rest f
  where
    rest acc = (symbol '*' *> factor >>= \f -> rest (acc * f))
           <|> (symbol '/' *> factor >>= \f -> rest (acc `div` f))
           <|> pure acc

factor :: Parser Int
factor = number <|> (symbol '(' *> expr <* symbol ')') <|> (negate <$> (symbol '-' *> factor))

parseExpr :: String -> Maybe Int
parseExpr s = case runParser (spaces *> expr) s of
  Just (v, "") -> Just v
  _ -> Nothing

data JSON = JNull | JBool Bool | JNum Int | JStr String | JArr [JSON] | JObj [(String, JSON)]
  deriving (Show, Eq)

json :: Parser JSON
json = token $
      JNull <$ string "null"
  <|> JBool True <$ string "true"
  <|> JBool False <$ string "false"
  <|> JNum <$> number
  <|> JStr <$> str
  <|> JArr <$> (symbol '[' *> sepBy json (symbol ',') <* symbol ']')
  <|> JObj <$> (symbol '{' *> sepBy pair (symbol ',') <* symbol '}')
  where
    pair = (,) <$> token str <* symbol ':' <*> json
    str = char '"' *> many (satisfy (/= '"')) <* char '"'

string :: String -> Parser String
string = traverse char

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = sepBy1 p sep <|> pure []

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

main :: IO ()
main = do
  print (map parseExpr ["1 + 2 * 3", "(1 + 2) * 3", "10 / 3 - -4", "2 *", "  42  "])
  print (runParser json "{\"a\": [1, true, null], \"b\": {\"c\": \"hi\"}}")
  print (runParser (many (symbol 'x')) "x x xy")
