-- Data.ByteString.Lazy.Char8: lazy ByteStrings of 8-bit characters (the
-- same type as strict ones in husk; see Data.ByteString.Lazy).
module Data.ByteString.Lazy.Char8 (
  module Data.ByteString.Char8, fromChunks, toChunks, fromStrict, toStrict, iterate, repeat, cycle
  ) where

import Prelude hiding (head, last, tail, init, null, length, map, reverse, foldl, foldr,
                       foldl1, foldr1, concat, concatMap, any, all, maximum, minimum, scanl,
                       take, drop, splitAt, takeWhile, dropWhile, span, break, lines, words,
                       unlines, unwords, elem, notElem, filter, zip, zipWith, unzip, replicate,
                       getLine, getContents, putStr, putStrLn, interact, readFile, writeFile,
                       appendFile, iterate, repeat, cycle)
import qualified Prelude as P
import Data.ByteString.Char8
import Data.ByteString.Lazy (fromChunks, toChunks, fromStrict, toStrict, cycle)

iterate :: (Char -> Char) -> Char -> ByteString
iterate f c = pack (P.iterate f c)

repeat :: Char -> ByteString
repeat c = pack (P.repeat c)
