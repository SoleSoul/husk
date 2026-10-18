-- Data.ByteString.Lazy: lazy ByteStrings.  In husk these are the same type
-- as strict ones (which are lazy already), so this module adds only the
-- conversions and the functions that make infinite ByteStrings.
module Data.ByteString.Lazy (
  module Data.ByteString, fromChunks, toChunks, fromStrict, toStrict, iterate, repeat, cycle
  ) where

import Prelude hiding (head, last, tail, init, null, length, map, reverse, foldl, foldr,
                       foldl1, foldr1, concat, concatMap, any, all, maximum, minimum,
                       take, drop, splitAt, takeWhile, dropWhile, span, break,
                       elem, notElem, filter, zip, zipWith, unzip, replicate,
                       getLine, getContents, putStr, interact, readFile, writeFile, appendFile,
                       iterate, repeat, cycle)
import qualified Prelude as P
import Data.ByteString
import Data.Word (Word8)
import Husk.ByteString (ByteString(..))

fromChunks :: [ByteString] -> ByteString
fromChunks = mconcat

toChunks :: ByteString -> [ByteString]
toChunks b = if null b then [] else [b]

fromStrict :: ByteString -> ByteString
fromStrict = id

toStrict :: ByteString -> ByteString
toStrict = id

iterate :: (Word8 -> Word8) -> Word8 -> ByteString
iterate f w = pack (P.iterate f w)

repeat :: Word8 -> ByteString
repeat w = pack (P.repeat w)

cycle :: ByteString -> ByteString
cycle b = BS (P.cycle (unBS b))
  where unBS (BS s) = s
