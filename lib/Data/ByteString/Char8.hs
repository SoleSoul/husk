-- Data.ByteString.Char8: ByteStrings as strings of 8-bit characters.
-- Usually imported qualified:  import qualified Data.ByteString.Char8 as B
-- (see Husk.ByteString for how ByteStrings are represented)
module Data.ByteString.Char8 (
  ByteString, pack, unpack, empty, singleton, cons, snoc, append, uncons, unsnoc,
  head, last, tail, init, null, length, map, reverse, intercalate, intersperse, transpose,
  foldl, foldl', foldr, foldl1, foldr1, concat, concatMap, any, all, maximum, minimum,
  scanl, take, takeEnd, drop, dropEnd, splitAt, takeWhile, takeWhileEnd, dropWhile,
  dropWhileEnd, dropSpace, strip, span, spanEnd, break, breakEnd, group, groupBy,
  inits, tails, stripPrefix, stripSuffix, split, splitWith, lines, words, unlines, unwords,
  isPrefixOf, isSuffixOf, isInfixOf, elem, notElem, find, filter, partition,
  index, indexMaybe, (!?), elemIndex, elemIndices, findIndex, findIndices, count,
  zip, zipWith, unzip, replicate, sort, copy, readInt, readInteger,
  getLine, getContents, putStr, putStrLn, interact, readFile, writeFile, appendFile,
  hGetLine, hGetContents, hPut, hPutStr, hPutStrLn
  ) where

import Prelude hiding (head, last, tail, init, null, length, map, reverse, foldl, foldr,
                       foldl1, foldr1, concat, concatMap, any, all, maximum, minimum, scanl,
                       take, drop, splitAt, takeWhile, dropWhile, span, break, lines, words,
                       unlines, unwords, elem, notElem, filter, zip, zipWith, unzip, replicate,
                       getLine, getContents, putStr, putStrLn, interact, readFile, writeFile,
                       appendFile)
import qualified Prelude as P
import qualified Data.List as L
import qualified Data.Char as C
import qualified System.IO as IO
import Husk.ByteString

infixl 9 !?

lift :: (String -> String) -> ByteString -> ByteString
lift f (BS s) = BS (f s)

pack :: String -> ByteString
pack = packChars

unpack :: ByteString -> String
unpack (BS s) = s

empty :: ByteString
empty = BS ""

singleton :: Char -> ByteString
singleton c = pack [c]

cons :: Char -> ByteString -> ByteString
cons c (BS s) = pack [c] <> BS s

snoc :: ByteString -> Char -> ByteString
snoc b c = b <> pack [c]

append :: ByteString -> ByteString -> ByteString
append = (<>)

uncons :: ByteString -> Maybe (Char, ByteString)
uncons (BS []) = Nothing
uncons (BS (c : cs)) = Just (c, BS cs)

unsnoc :: ByteString -> Maybe (ByteString, Char)
unsnoc (BS []) = Nothing
unsnoc (BS s) = Just (BS (P.init s), P.last s)

head :: ByteString -> Char
head (BS s) = P.head s

last :: ByteString -> Char
last (BS s) = P.last s

tail :: ByteString -> ByteString
tail = lift P.tail

init :: ByteString -> ByteString
init = lift P.init

null :: ByteString -> Bool
null (BS s) = P.null s

length :: ByteString -> Int
length (BS s) = P.length s

map :: (Char -> Char) -> ByteString -> ByteString
map f (BS s) = pack (P.map f s)

reverse :: ByteString -> ByteString
reverse = lift P.reverse

intercalate :: ByteString -> [ByteString] -> ByteString
intercalate (BS sep) bs = BS (L.intercalate sep (P.map unpack bs))

intersperse :: Char -> ByteString -> ByteString
intersperse c (BS s) = pack (L.intersperse c s)

transpose :: [ByteString] -> [ByteString]
transpose bs = P.map BS (L.transpose (P.map unpack bs))

foldl :: (a -> Char -> a) -> a -> ByteString -> a
foldl f z (BS s) = P.foldl f z s

foldl' :: (a -> Char -> a) -> a -> ByteString -> a
foldl' f z (BS s) = L.foldl' f z s

foldr :: (Char -> a -> a) -> a -> ByteString -> a
foldr f z (BS s) = P.foldr f z s

foldl1 :: (Char -> Char -> Char) -> ByteString -> Char
foldl1 f (BS s) = P.foldl1 f s

foldr1 :: (Char -> Char -> Char) -> ByteString -> Char
foldr1 f (BS s) = P.foldr1 f s

concat :: [ByteString] -> ByteString
concat = mconcat

concatMap :: (Char -> ByteString) -> ByteString -> ByteString
concatMap f (BS s) = mconcat (P.map f s)

any :: (Char -> Bool) -> ByteString -> Bool
any p (BS s) = P.any p s

all :: (Char -> Bool) -> ByteString -> Bool
all p (BS s) = P.all p s

maximum :: ByteString -> Char
maximum (BS s) = P.maximum s

minimum :: ByteString -> Char
minimum (BS s) = P.minimum s

scanl :: (Char -> Char -> Char) -> Char -> ByteString -> ByteString
scanl f z (BS s) = pack (P.scanl f z s)

take :: Int -> ByteString -> ByteString
take n = lift (P.take n)

takeEnd :: Int -> ByteString -> ByteString
takeEnd n (BS s) = BS (P.drop (P.length s - n) s)

drop :: Int -> ByteString -> ByteString
drop n = lift (P.drop n)

dropEnd :: Int -> ByteString -> ByteString
dropEnd n (BS s) = BS (P.take (P.length s - n) s)

splitAt :: Int -> ByteString -> (ByteString, ByteString)
splitAt n (BS s) = let (a, b) = P.splitAt n s in (BS a, BS b)

takeWhile :: (Char -> Bool) -> ByteString -> ByteString
takeWhile p = lift (P.takeWhile p)

takeWhileEnd :: (Char -> Bool) -> ByteString -> ByteString
takeWhileEnd p = lift (P.reverse . P.takeWhile p . P.reverse)

dropWhile :: (Char -> Bool) -> ByteString -> ByteString
dropWhile p = lift (P.dropWhile p)

dropWhileEnd :: (Char -> Bool) -> ByteString -> ByteString
dropWhileEnd p = lift (L.dropWhileEnd p)

dropSpace :: ByteString -> ByteString
dropSpace = dropWhile C.isSpace

strip :: ByteString -> ByteString
strip = dropWhileEnd C.isSpace . dropWhile C.isSpace

span :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
span p (BS s) = let (a, b) = P.span p s in (BS a, BS b)

spanEnd :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
spanEnd p b = (dropWhileEnd p b, takeWhileEnd p b)

break :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
break p = span (not . p)

breakEnd :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
breakEnd p = spanEnd (not . p)

group :: ByteString -> [ByteString]
group (BS s) = P.map BS (L.group s)

groupBy :: (Char -> Char -> Bool) -> ByteString -> [ByteString]
groupBy f (BS s) = P.map BS (L.groupBy f s)

inits :: ByteString -> [ByteString]
inits (BS s) = P.map BS (L.inits s)

tails :: ByteString -> [ByteString]
tails (BS s) = P.map BS (L.tails s)

stripPrefix :: ByteString -> ByteString -> Maybe ByteString
stripPrefix (BS a) (BS b) = fmap BS (L.stripPrefix a b)

stripSuffix :: ByteString -> ByteString -> Maybe ByteString
stripSuffix (BS a) (BS b) = fmap (BS . P.reverse) (L.stripPrefix (P.reverse a) (P.reverse b))

-- split ',' "a,b,,c" == ["a","b","","c"];  split ',' "" == []
split :: Char -> ByteString -> [ByteString]
split c = splitWith (== c)

splitWith :: (Char -> Bool) -> ByteString -> [ByteString]
splitWith p (BS []) = []
splitWith p (BS s) = go s
  where go s = case P.break p s of
                 (a, []) -> [BS a]
                 (a, _ : rest) -> BS a : go rest

lines :: ByteString -> [ByteString]
lines (BS s) = P.map BS (P.lines s)

words :: ByteString -> [ByteString]
words (BS s) = P.map BS (P.words s)

unlines :: [ByteString] -> ByteString
unlines bs = BS (P.unlines (P.map unpack bs))

unwords :: [ByteString] -> ByteString
unwords bs = BS (P.unwords (P.map unpack bs))

isPrefixOf :: ByteString -> ByteString -> Bool
isPrefixOf (BS a) (BS b) = L.isPrefixOf a b

isSuffixOf :: ByteString -> ByteString -> Bool
isSuffixOf (BS a) (BS b) = L.isSuffixOf a b

isInfixOf :: ByteString -> ByteString -> Bool
isInfixOf (BS a) (BS b) = L.isInfixOf a b

elem :: Char -> ByteString -> Bool
elem c (BS s) = P.elem c s

notElem :: Char -> ByteString -> Bool
notElem c (BS s) = P.notElem c s

find :: (Char -> Bool) -> ByteString -> Maybe Char
find p (BS s) = L.find p s

filter :: (Char -> Bool) -> ByteString -> ByteString
filter p = lift (P.filter p)

partition :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
partition p (BS s) = let (a, b) = L.partition p s in (BS a, BS b)

index :: ByteString -> Int -> Char
index (BS s) n = s !! n

indexMaybe :: ByteString -> Int -> Maybe Char
indexMaybe (BS s) n
  | n < 0 = Nothing
  | otherwise = case P.drop n s of
                  (c : _) -> Just c
                  [] -> Nothing

(!?) :: ByteString -> Int -> Maybe Char
(!?) = indexMaybe

elemIndex :: Char -> ByteString -> Maybe Int
elemIndex c (BS s) = L.elemIndex c s

elemIndices :: Char -> ByteString -> [Int]
elemIndices c (BS s) = L.elemIndices c s

findIndex :: (Char -> Bool) -> ByteString -> Maybe Int
findIndex p (BS s) = L.findIndex p s

findIndices :: (Char -> Bool) -> ByteString -> [Int]
findIndices p (BS s) = L.findIndices p s

count :: Char -> ByteString -> Int
count c (BS s) = P.length (P.filter (== c) s)

zip :: ByteString -> ByteString -> [(Char, Char)]
zip (BS a) (BS b) = P.zip a b

zipWith :: (Char -> Char -> a) -> ByteString -> ByteString -> [a]
zipWith f (BS a) (BS b) = P.zipWith f a b

unzip :: [(Char, Char)] -> (ByteString, ByteString)
unzip ps = let (a, b) = P.unzip ps in (pack a, pack b)

replicate :: Int -> Char -> ByteString
replicate n c = pack (P.replicate n c)

sort :: ByteString -> ByteString
sort = lift L.sort

copy :: ByteString -> ByteString
copy = id

-- reads an optionally signed decimal number at the start (no leading spaces)
readInt :: ByteString -> Maybe (Int, ByteString)
readInt b = fmap (\(n, rest) -> (fromInteger n, rest)) (readInteger b)

readInteger :: ByteString -> Maybe (Integer, ByteString)
readInteger (BS s) = case s of
  ('-' : rest) -> fmap (\(n, r) -> (negate n, r)) (digits rest)
  ('+' : rest) -> digits rest
  _ -> digits s
  where
    digits s = case P.span C.isDigit s of
      ([], _) -> Nothing
      (ds, rest) -> Just (L.foldl' (\acc d -> acc * 10 + toInteger (C.digitToInt d)) 0 ds, BS rest)

getLine :: IO ByteString
getLine = fmap (BS . encodeUtf8) P.getLine

getContents :: IO ByteString
getContents = fmap (BS . encodeUtf8) P.getContents

putStr :: ByteString -> IO ()
putStr (BS s) = P.putStr (decodeUtf8 s)

putStrLn :: ByteString -> IO ()
putStrLn (BS s) = P.putStrLn (decodeUtf8 s)

interact :: (ByteString -> ByteString) -> IO ()
interact f = P.interact (decodeUtf8 . unpack . f . BS . encodeUtf8)

readFile :: FilePath -> IO ByteString
readFile p = fmap BS (primReadFileBytes p)

writeFile :: FilePath -> ByteString -> IO ()
writeFile p (BS s) = primWriteFileBytes p s

appendFile :: FilePath -> ByteString -> IO ()
appendFile p (BS s) = primAppendFileBytes p s

hGetLine :: IO.Handle -> IO ByteString
hGetLine h = fmap (BS . encodeUtf8) (IO.hGetLine h)

hGetContents :: IO.Handle -> IO ByteString
hGetContents h = fmap (BS . encodeUtf8) (IO.hGetContents h)

hPut :: IO.Handle -> ByteString -> IO ()
hPut h (BS s) = IO.hPutStr h (decodeUtf8 s)

hPutStr :: IO.Handle -> ByteString -> IO ()
hPutStr = hPut

hPutStrLn :: IO.Handle -> ByteString -> IO ()
hPutStrLn h b = hPut h b >> IO.hPutStr h "\n"
