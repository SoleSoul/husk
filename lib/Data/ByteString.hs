-- Data.ByteString: strings of bytes (Word8).
-- Usually imported qualified:  import qualified Data.ByteString as B
-- The same functions with Char instead of Word8 are in Data.ByteString.Char8
-- (see Husk.ByteString for how ByteStrings are represented).
module Data.ByteString (
  ByteString, pack, unpack, empty, singleton, cons, snoc, append, uncons, unsnoc,
  head, last, tail, init, null, length, map, reverse, intercalate, intersperse, transpose,
  foldl, foldl', foldr, foldl1, foldr1, concat, concatMap, any, all, maximum, minimum,
  take, takeEnd, drop, dropEnd, splitAt, takeWhile, takeWhileEnd, dropWhile, dropWhileEnd,
  span, spanEnd, break, breakEnd, group, groupBy, inits, tails, stripPrefix, stripSuffix,
  split, splitWith, isPrefixOf, isSuffixOf, isInfixOf, elem, notElem, find, filter,
  partition, index, indexMaybe, (!?), elemIndex, elemIndices, findIndex, findIndices, count,
  zip, zipWith, unzip, replicate, sort, copy,
  getLine, getContents, putStr, interact, readFile, writeFile, appendFile,
  hGetLine, hGetContents, hPut, hPutStr
  ) where

import Prelude hiding (head, last, tail, init, null, length, map, reverse, foldl, foldr,
                       foldl1, foldr1, concat, concatMap, any, all, maximum, minimum,
                       take, drop, splitAt, takeWhile, dropWhile, span, break,
                       elem, notElem, filter, zip, zipWith, unzip, replicate,
                       getLine, getContents, putStr, interact, readFile, writeFile, appendFile)
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Char8 (ByteString, empty, append, tail, init, null, length, reverse,
                              intercalate, transpose, concat, take, takeEnd, drop, dropEnd,
                              splitAt, group, inits, tails, stripPrefix, stripSuffix,
                              isPrefixOf, isSuffixOf, isInfixOf, sort, copy,
                              getLine, getContents, putStr, interact, readFile, writeFile,
                              appendFile, hGetLine, hGetContents, hPut, hPutStr)
import Data.Word (Word8)

infixl 9 !?

w2c :: Word8 -> Char
w2c w = toEnum (fromIntegral w)

c2w :: Char -> Word8
c2w = fromIntegral . fromEnum

p1 :: (Word8 -> a) -> Char -> a
p1 f = f . c2w

pack :: [Word8] -> ByteString
pack ws = C.pack (fmap w2c ws)

unpack :: ByteString -> [Word8]
unpack b = fmap c2w (C.unpack b)

singleton :: Word8 -> ByteString
singleton = C.singleton . w2c

cons :: Word8 -> ByteString -> ByteString
cons w = C.cons (w2c w)

snoc :: ByteString -> Word8 -> ByteString
snoc b w = C.snoc b (w2c w)

uncons :: ByteString -> Maybe (Word8, ByteString)
uncons b = fmap (\(c, r) -> (c2w c, r)) (C.uncons b)

unsnoc :: ByteString -> Maybe (ByteString, Word8)
unsnoc b = fmap (\(r, c) -> (r, c2w c)) (C.unsnoc b)

head :: ByteString -> Word8
head = c2w . C.head

last :: ByteString -> Word8
last = c2w . C.last

map :: (Word8 -> Word8) -> ByteString -> ByteString
map f = C.map (w2c . f . c2w)

intersperse :: Word8 -> ByteString -> ByteString
intersperse w = C.intersperse (w2c w)

foldl :: (a -> Word8 -> a) -> a -> ByteString -> a
foldl f = C.foldl (\a c -> f a (c2w c))

foldl' :: (a -> Word8 -> a) -> a -> ByteString -> a
foldl' f = C.foldl' (\a c -> f a (c2w c))

foldr :: (Word8 -> a -> a) -> a -> ByteString -> a
foldr f = C.foldr (p1 f)

foldl1 :: (Word8 -> Word8 -> Word8) -> ByteString -> Word8
foldl1 f b = c2w (C.foldl1 (\x y -> w2c (f (c2w x) (c2w y))) b)

foldr1 :: (Word8 -> Word8 -> Word8) -> ByteString -> Word8
foldr1 f b = c2w (C.foldr1 (\x y -> w2c (f (c2w x) (c2w y))) b)

concatMap :: (Word8 -> ByteString) -> ByteString -> ByteString
concatMap f = C.concatMap (p1 f)

any :: (Word8 -> Bool) -> ByteString -> Bool
any p = C.any (p1 p)

all :: (Word8 -> Bool) -> ByteString -> Bool
all p = C.all (p1 p)

maximum :: ByteString -> Word8
maximum = c2w . C.maximum

minimum :: ByteString -> Word8
minimum = c2w . C.minimum

takeWhile :: (Word8 -> Bool) -> ByteString -> ByteString
takeWhile p = C.takeWhile (p1 p)

takeWhileEnd :: (Word8 -> Bool) -> ByteString -> ByteString
takeWhileEnd p = C.takeWhileEnd (p1 p)

dropWhile :: (Word8 -> Bool) -> ByteString -> ByteString
dropWhile p = C.dropWhile (p1 p)

dropWhileEnd :: (Word8 -> Bool) -> ByteString -> ByteString
dropWhileEnd p = C.dropWhileEnd (p1 p)

span :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
span p = C.span (p1 p)

spanEnd :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
spanEnd p = C.spanEnd (p1 p)

break :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
break p = C.break (p1 p)

breakEnd :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
breakEnd p = C.breakEnd (p1 p)

groupBy :: (Word8 -> Word8 -> Bool) -> ByteString -> [ByteString]
groupBy f = C.groupBy (\x y -> f (c2w x) (c2w y))

split :: Word8 -> ByteString -> [ByteString]
split w = C.split (w2c w)

splitWith :: (Word8 -> Bool) -> ByteString -> [ByteString]
splitWith p = C.splitWith (p1 p)

elem :: Word8 -> ByteString -> Bool
elem w = C.elem (w2c w)

notElem :: Word8 -> ByteString -> Bool
notElem w = C.notElem (w2c w)

find :: (Word8 -> Bool) -> ByteString -> Maybe Word8
find p b = fmap c2w (C.find (p1 p) b)

filter :: (Word8 -> Bool) -> ByteString -> ByteString
filter p = C.filter (p1 p)

partition :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
partition p = C.partition (p1 p)

index :: ByteString -> Int -> Word8
index b n = c2w (C.index b n)

indexMaybe :: ByteString -> Int -> Maybe Word8
indexMaybe b n = fmap c2w (C.indexMaybe b n)

(!?) :: ByteString -> Int -> Maybe Word8
(!?) = indexMaybe

elemIndex :: Word8 -> ByteString -> Maybe Int
elemIndex w = C.elemIndex (w2c w)

elemIndices :: Word8 -> ByteString -> [Int]
elemIndices w = C.elemIndices (w2c w)

findIndex :: (Word8 -> Bool) -> ByteString -> Maybe Int
findIndex p = C.findIndex (p1 p)

findIndices :: (Word8 -> Bool) -> ByteString -> [Int]
findIndices p = C.findIndices (p1 p)

count :: Word8 -> ByteString -> Int
count w = C.count (w2c w)

zip :: ByteString -> ByteString -> [(Word8, Word8)]
zip a b = fmap (\(x, y) -> (c2w x, c2w y)) (C.zip a b)

zipWith :: (Word8 -> Word8 -> a) -> ByteString -> ByteString -> [a]
zipWith f = C.zipWith (\x y -> f (c2w x) (c2w y))

unzip :: [(Word8, Word8)] -> (ByteString, ByteString)
unzip ps = C.unzip (fmap (\(x, y) -> (w2c x, w2c y)) ps)

replicate :: Int -> Word8 -> ByteString
replicate n w = C.replicate n (w2c w)
