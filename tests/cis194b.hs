{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
-- CIS 194 (Spring 2013) homework 7, 8, 10, 11 and 12, with my own
-- solutions.  The course's support files are replaced by small versions
-- written for this test.
import Data.Char
import Data.List (sort)
import Data.Tree
import Control.Applicative
import Control.Monad.Random

-- HW7: join lists (Sized and Buffer stand in for the course's files)
newtype Size = Size Int deriving (Eq, Ord, Show, Num)

instance Semigroup Size where
  (<>) = (+)

instance Monoid Size where
  mempty = Size 0

class Sized a where
  size :: a -> Size

instance Sized Size where
  size = id

instance Sized b => Sized (a, b) where
  size = size . snd

getSize :: Size -> Int
getSize (Size i) = i

newtype Score = Score Int deriving (Eq, Ord, Show, Num)

instance Semigroup Score where
  (<>) = (+)

instance Monoid Score where
  mempty = 0

score :: Char -> Score
score c
  | u `elem` "AEILNORSTU" = 1
  | u `elem` "DG" = 2
  | u `elem` "BCMP" = 3
  | u `elem` "FHVWY" = 4
  | u `elem` "K" = 5
  | u `elem` "JX" = 8
  | u `elem` "QZ" = 10
  | otherwise = 0
  where u = toUpper c

scoreString :: String -> Score
scoreString = sum . map score

data JoinList m a = Empty | Single m a | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
a +++ b = Append (tag a <> tag b) a b

sz :: (Sized b, Monoid b) => JoinList b a -> Int
sz = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i (Single _ a) = if i == 0 then Just a else Nothing
indexJ i (Append _ l r)
  | i < 0 || i >= sz l + sz r = Nothing
  | i < sz l = indexJ i l
  | otherwise = indexJ (i - sz l) r

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n jl | n <= 0 = jl
dropJ _ Empty = Empty
dropJ _ (Single _ _) = Empty
dropJ n (Append _ l r)
  | n >= sz l = dropJ (n - sz l) r
  | otherwise = dropJ n l +++ r

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n _ | n <= 0 = Empty
takeJ _ Empty = Empty
takeJ _ s@(Single _ _) = s
takeJ n (Append _ l r)
  | n >= sz l = l +++ takeJ (n - sz l) r
  | otherwise = takeJ n l

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l r) = jlToList l ++ jlToList r

class Buffer b where
  toString :: b -> String
  fromString :: String -> b
  line :: Int -> b -> Maybe String
  replaceLine :: Int -> String -> b -> b
  numLines :: b -> Int
  value :: b -> Int

instance Buffer (JoinList (Score, Size) String) where
  toString = unlines . jlToList
  fromString = foldr (+++) Empty . map (\l -> Single (scoreString l, Size 1) l) . lines
  line = indexJ
  replaceLine n l b = takeJ n b +++ fromString l +++ dropJ (n + 1) b
  numLines = sz
  value b = let Score s = fst (tag b) in s

-- HW8: party planning (Employee stands in for the course's file)
data Employee = Emp { empName :: String, empFun :: Integer } deriving (Show, Read, Eq)

data GuestList = GL [Employee] Integer deriving (Show, Eq)

instance Ord GuestList where
  compare (GL _ f1) (GL _ f2) = compare f1 f2

testCompany :: Tree Employee
testCompany = Node (Emp "Stan" 9)
  [ Node (Emp "Bob" 2) [Node (Emp "Joe" 5) [Node (Emp "John" 1) [], Node (Emp "Sue" 5) []], Node (Emp "Fred" 3) []]
  , Node (Emp "Sarah" 17) [Node (Emp "Sam" 4) []] ]

glCons :: Employee -> GuestList -> GuestList
glCons e (GL es f) = GL (e : es) (f + empFun e)

instance Semigroup GuestList where
  GL a f <> GL b g = GL (a ++ b) (f + g)

instance Monoid GuestList where
  mempty = GL [] 0

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node x ts) = f x (map (treeFold f) ts)

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss gls = (glCons boss (mconcat (map snd gls)), mconcat (map (uncurry max) gls))

maxFun :: Tree Employee -> GuestList
maxFun = uncurry max . treeFold nextLevel

-- HW10 and HW11: applicative parsing
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where f (x : xs) | p x = Just (x, xs)
        f _ = Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

posInt :: Parser Integer
posInt = Parser f
  where f xs | null ns = Nothing
             | otherwise = Just (read ns, rest)
          where (ns, rest) = span isDigit xs

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Functor Parser where
  fmap f (Parser p) = Parser (fmap (first f) . p)

instance Applicative Parser where
  pure x = Parser (\s -> Just (x, s))
  Parser pf <*> Parser px = Parser (\s -> case pf s of
    Nothing -> Nothing
    Just (f, rest) -> first f <$> px rest)

instance Alternative Parser where
  empty = Parser (const Nothing)
  Parser p <|> Parser q = Parser (\s -> p s <|> q s)

abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

intPair :: Parser [Integer]
intPair = (\a _ b -> [a, b]) <$> posInt <*> char ' ' <*> posInt

intOrUppercase :: Parser ()
intOrUppercase = () <$ posInt <|> () <$ satisfy isUpper

zeroOrMore, oneOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []
oneOrMore p = (:) <$> p <*> zeroOrMore p

spaces, ident :: Parser String
spaces = zeroOrMore (satisfy isSpace)
ident = (:) <$> satisfy isAlpha <*> zeroOrMore (satisfy isAlphaNum)

data Atom = N Integer | I String deriving Show
data SExpr = A Atom | Comb [SExpr] deriving Show

parseSExpr :: Parser SExpr
parseSExpr = spaces *> (A <$> (N <$> posInt <|> I <$> ident) <|> Comb <$> (char '(' *> zeroOrMore parseSExpr <* spaces <* char ')')) <* spaces

-- HW12: Risk with the MonadRandom monad (DieValue as in the course's Risk.hs)
newtype DieValue = DV { unDV :: Int } deriving (Eq, Ord, Show, Num)

instance Random DieValue where
  random = first DV . randomR (1, 6)
  randomR (low, hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

data Battlefield = Battlefield { attackers :: Int, defenders :: Int } deriving Show

battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield a d) = do
  as <- replicateM (min 3 (a - 1)) die
  ds <- replicateM (min 2 d) die
  let results = zipWith (>) (reverse (sort as)) (reverse (sort ds))
  return (Battlefield (a - length (filter not results)) (d - length (filter id results)))

invade :: Battlefield -> Rand StdGen Battlefield
invade bf@(Battlefield a d)
  | d == 0 || a < 2 = return bf
  | otherwise = battle bf >>= invade

successProb :: Battlefield -> Rand StdGen Double
successProb bf = do
  results <- replicateM 1000 (invade bf)
  return (fromIntegral (length (filter ((== 0) . defenders) results)) / 1000)

main :: IO ()
main = do
  let yeah = foldr1 (+++) [Single (Size 1) c | c <- "yeah"]
  print yeah
  print ([indexJ i yeah | i <- [-1 .. 4]], [jlToList (dropJ n yeah) | n <- [0 .. 5]], jlToList (takeJ 3 yeah))
  print (Single (scoreString "yay ") "yay " +++ Single (scoreString "haskell!") "haskell!")
  let b = fromString "This buffer is for notes\nyou don't want to save" :: JoinList (Score, Size) String
  print (numLines b, value b, line 1 b, toString (replaceLine 0 "Hello" b))
  let GL es f = maxFun testCompany
  print (f, sort (map empName es))
  print (maxFun (read (show testCompany)) == maxFun testCompany)
  print (runParser abParser "abcdef", runParser abParser "aebcdf", runParser intPair "12 34")
  print (runParser intOrUppercase "342abcd", runParser intOrUppercase "XYZ", runParser intOrUppercase "foo")
  print (runParser (zeroOrMore (satisfy isUpper)) "ABCdEfgH", runParser (oneOrMore (satisfy isUpper)) "abcdeFGh")
  mapM_ (print . runParser parseSExpr) ["(bar (foo) 3 5 874)", "(((lambda x (lambda y (plus x y))) 3) 5)", "(unclosed"]
  let g = mkStdGen 2013
      ds = evalRand (replicateM 50 die) g
  print (all (\(DV v) -> v >= 1 && v <= 6) ds, ds == evalRand (replicateM 50 die) g)
  let r = evalRand (invade (Battlefield 10 10)) g
  print (defenders r == 0 || attackers r < 2)
  let p = evalRand (successProb (Battlefield 10 10)) g
  print (p > 0.35 && p < 0.6)
