-- Data.Text: a basic Text type (implemented over String).
-- Usually imported qualified:  import qualified Data.Text as T
-- With {-# LANGUAGE OverloadedStrings #-} string literals can be Text.
module Data.Text (
  Text, pack, unpack, empty, singleton, append, cons, snoc, uncons, unsnoc,
  head, last, tail, init, null, length, compareLength, map, intercalate, intersperse,
  reverse, replace, toUpper, toLower, toTitle, concat, concatMap, any, all, maximum, minimum,
  foldl, foldl', foldr, take, takeEnd, drop, dropEnd, takeWhile, takeWhileEnd, dropWhile,
  dropWhileEnd, dropAround, strip, stripStart, stripEnd, splitAt, breakOn, breakOnEnd,
  break, span, group, groupBy, inits, tails, splitOn, split, chunksOf, lines, words,
  unlines, unwords, isPrefixOf, isSuffixOf, isInfixOf, stripPrefix, stripSuffix,
  filter, find, elem, partition, index, findIndex, count, zip, zipWith, replicate,
  justifyLeft, justifyRight, center, transpose
  ) where

import Prelude hiding (head, last, tail, init, null, length, map, reverse, concat, concatMap,
                       any, all, maximum, minimum, foldl, foldr, take, drop, takeWhile,
                       dropWhile, splitAt, break, span, lines, words, unlines, unwords,
                       filter, elem, zip, zipWith, replicate)
import qualified Prelude as P
import qualified Data.List as L
import qualified Data.Char as C
import qualified Data.List.Split as S
import Husk.Base (IsString(..))

newtype Text = Text String

instance Eq Text where
  Text a == Text b = a == b

instance Ord Text where
  compare (Text a) (Text b) = compare a b

instance Show Text where
  showsPrec d (Text s) = showsPrec d s

instance Read Text where
  readsPrec d s = [(Text x, r) | (x, r) <- readsPrec d s]

instance Semigroup Text where
  Text a <> Text b = Text (a ++ b)

instance Monoid Text where
  mempty = Text ""

instance IsString Text where
  fromString = Text

pack :: String -> Text
pack = Text

unpack :: Text -> String
unpack (Text s) = s

lift :: (String -> String) -> Text -> Text
lift f (Text s) = Text (f s)

empty :: Text
empty = Text ""

singleton :: Char -> Text
singleton c = Text [c]

append :: Text -> Text -> Text
append (Text a) (Text b) = Text (a ++ b)

cons :: Char -> Text -> Text
cons c (Text s) = Text (c : s)

snoc :: Text -> Char -> Text
snoc (Text s) c = Text (s ++ [c])

uncons :: Text -> Maybe (Char, Text)
uncons (Text []) = Nothing
uncons (Text (c:cs)) = Just (c, Text cs)

unsnoc :: Text -> Maybe (Text, Char)
unsnoc (Text []) = Nothing
unsnoc (Text s) = Just (Text (P.init s), P.last s)

head :: Text -> Char
head (Text s) = P.head s

last :: Text -> Char
last (Text s) = P.last s

tail :: Text -> Text
tail = lift P.tail

init :: Text -> Text
init = lift P.init

null :: Text -> Bool
null (Text s) = P.null s

length :: Text -> Int
length (Text s) = P.length s

compareLength :: Text -> Int -> Ordering
compareLength t n = compare (length t) n

map :: (Char -> Char) -> Text -> Text
map f = lift (P.map f)

intercalate :: Text -> [Text] -> Text
intercalate (Text sep) ts = Text (L.intercalate sep (P.map unpack ts))

intersperse :: Char -> Text -> Text
intersperse c = lift (L.intersperse c)

reverse :: Text -> Text
reverse = lift P.reverse

-- | replace needle replacement haystack
replace :: Text -> Text -> Text -> Text
replace (Text needle) (Text rep) (Text hay) = Text (L.intercalate rep (S.splitOn needle hay))

toUpper :: Text -> Text
toUpper = map C.toUpper

toLower :: Text -> Text
toLower = map C.toLower

toTitle :: Text -> Text
toTitle (Text s) = Text (go True s)
  where go _ [] = []
        go up (c:cs) | C.isAlpha c = (if up then C.toUpper c else c) : go False cs
                     | otherwise = c : go True cs

concat :: [Text] -> Text
concat ts = Text (P.concatMap unpack ts)

concatMap :: (Char -> Text) -> Text -> Text
concatMap f (Text s) = Text (P.concatMap (unpack . f) s)

any :: (Char -> Bool) -> Text -> Bool
any p (Text s) = P.any p s

all :: (Char -> Bool) -> Text -> Bool
all p (Text s) = P.all p s

maximum :: Text -> Char
maximum (Text s) = P.maximum s

minimum :: Text -> Char
minimum (Text s) = P.minimum s

foldl :: (a -> Char -> a) -> a -> Text -> a
foldl f z (Text s) = P.foldl f z s

foldl' :: (a -> Char -> a) -> a -> Text -> a
foldl' f z (Text s) = L.foldl' f z s

foldr :: (Char -> a -> a) -> a -> Text -> a
foldr f z (Text s) = P.foldr f z s

take :: Int -> Text -> Text
take n = lift (P.take n)

takeEnd :: Int -> Text -> Text
takeEnd n (Text s) = Text (P.drop (P.length s - n) s)

drop :: Int -> Text -> Text
drop n = lift (P.drop n)

dropEnd :: Int -> Text -> Text
dropEnd n (Text s) = Text (P.take (P.length s - n) s)

takeWhile :: (Char -> Bool) -> Text -> Text
takeWhile p = lift (P.takeWhile p)

takeWhileEnd :: (Char -> Bool) -> Text -> Text
takeWhileEnd p = lift (P.reverse . P.takeWhile p . P.reverse)

dropWhile :: (Char -> Bool) -> Text -> Text
dropWhile p = lift (P.dropWhile p)

dropWhileEnd :: (Char -> Bool) -> Text -> Text
dropWhileEnd p = lift (L.dropWhileEnd p)

dropAround :: (Char -> Bool) -> Text -> Text
dropAround p = dropWhile p . dropWhileEnd p

strip :: Text -> Text
strip = dropAround C.isSpace

stripStart :: Text -> Text
stripStart = dropWhile C.isSpace

stripEnd :: Text -> Text
stripEnd = dropWhileEnd C.isSpace

splitAt :: Int -> Text -> (Text, Text)
splitAt n (Text s) = let (a, b) = P.splitAt n s in (Text a, Text b)

-- | breakOn "::" "a::b::c" == ("a", "::b::c")
breakOn :: Text -> Text -> (Text, Text)
breakOn (Text needle) (Text hay) = go [] hay
  where go acc s | needle `L.isPrefixOf` s || P.null s = (Text (P.reverse acc), Text s)
        go acc (c:cs) = go (c : acc) cs

breakOnEnd :: Text -> Text -> (Text, Text)
breakOnEnd needle hay =
  let (a, b) = breakOn (reverse needle) (reverse hay)
  in if null b then (empty, hay)
     else (reverse b, reverse a)

break :: (Char -> Bool) -> Text -> (Text, Text)
break p (Text s) = let (a, b) = P.break p s in (Text a, Text b)

span :: (Char -> Bool) -> Text -> (Text, Text)
span p (Text s) = let (a, b) = P.span p s in (Text a, Text b)

group :: Text -> [Text]
group (Text s) = P.map Text (L.group s)

groupBy :: (Char -> Char -> Bool) -> Text -> [Text]
groupBy f (Text s) = P.map Text (L.groupBy f s)

inits :: Text -> [Text]
inits (Text s) = P.map Text (L.inits s)

tails :: Text -> [Text]
tails (Text s) = P.map Text (L.tails s)

splitOn :: Text -> Text -> [Text]
splitOn (Text sep) (Text s) = P.map Text (S.splitOn sep s)

split :: (Char -> Bool) -> Text -> [Text]
split p (Text s) = P.map Text (S.splitWhen p s)

chunksOf :: Int -> Text -> [Text]
chunksOf n (Text s) = P.map Text (S.chunksOf n s)

lines :: Text -> [Text]
lines (Text s) = P.map Text (P.lines s)

words :: Text -> [Text]
words (Text s) = P.map Text (P.words s)

unlines :: [Text] -> Text
unlines ts = Text (P.unlines (P.map unpack ts))

unwords :: [Text] -> Text
unwords ts = Text (P.unwords (P.map unpack ts))

isPrefixOf :: Text -> Text -> Bool
isPrefixOf (Text a) (Text b) = L.isPrefixOf a b

isSuffixOf :: Text -> Text -> Bool
isSuffixOf (Text a) (Text b) = L.isSuffixOf a b

isInfixOf :: Text -> Text -> Bool
isInfixOf (Text a) (Text b) = L.isInfixOf a b

stripPrefix :: Text -> Text -> Maybe Text
stripPrefix (Text p) (Text s) = fmap Text (L.stripPrefix p s)

stripSuffix :: Text -> Text -> Maybe Text
stripSuffix (Text p) (Text s) = fmap (Text . P.reverse) (L.stripPrefix (P.reverse p) (P.reverse s))

filter :: (Char -> Bool) -> Text -> Text
filter p = lift (P.filter p)

find :: (Char -> Bool) -> Text -> Maybe Char
find p (Text s) = L.find p s

elem :: Char -> Text -> Bool
elem c (Text s) = P.elem c s

partition :: (Char -> Bool) -> Text -> (Text, Text)
partition p (Text s) = let (a, b) = L.partition p s in (Text a, Text b)

index :: Text -> Int -> Char
index (Text s) i = s !! i

findIndex :: (Char -> Bool) -> Text -> Maybe Int
findIndex p (Text s) = L.findIndex p s

count :: Text -> Text -> Int
count (Text needle) (Text hay) = P.length (S.splitOn needle hay) - 1

zip :: Text -> Text -> [(Char, Char)]
zip (Text a) (Text b) = P.zip a b

zipWith :: (Char -> Char -> Char) -> Text -> Text -> Text
zipWith f (Text a) (Text b) = Text (P.zipWith f a b)

replicate :: Int -> Text -> Text
replicate n (Text s) = Text (P.concat (P.replicate n s))

justifyLeft :: Int -> Char -> Text -> Text
justifyLeft n c (Text s) = Text (s ++ P.replicate (n - P.length s) c)

justifyRight :: Int -> Char -> Text -> Text
justifyRight n c (Text s) = Text (P.replicate (n - P.length s) c ++ s)

center :: Int -> Char -> Text -> Text
center n c (Text s) = let total = n - P.length s
                          l = total `div` 2
                          r = total - l
                      in if total <= 0 then Text s else Text (P.replicate r c ++ s ++ P.replicate l c)

transpose :: [Text] -> [Text]
transpose ts = P.map Text (L.transpose (P.map unpack ts))
