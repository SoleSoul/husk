-- Husk.ByteString: the ByteString type shared by Data.ByteString,
-- Data.ByteString.Char8 and their lazy versions.
--
-- A ByteString is a String whose characters are all below 256, one per
-- byte, so it is no faster than String.  Files are read and written byte
-- for byte; the terminal and handles are text, converted with UTF-8.
module Husk.ByteString where
import Husk.Base

newtype ByteString = BS String

instance Eq ByteString where
  BS a == BS b = a == b

instance Ord ByteString where
  compare (BS a) (BS b) = compare a b

-- shown like a string, as GHC does
instance Show ByteString where
  showsPrec d (BS s) = showsPrec d s

instance Read ByteString where
  readsPrec d s = [(packChars x, r) | (x, r) <- readsPrec d s]

instance Semigroup ByteString where
  BS a <> BS b = BS (a ++ b)

instance Monoid ByteString where
  mempty = BS ""

instance IsString ByteString where
  fromString = packChars

unBS :: ByteString -> String
unBS (BS s) = s

-- characters are truncated to 8 bits, as in Data.ByteString.Char8.pack
packChars :: String -> ByteString
packChars s = BS (map (\c -> if c < '\256' then c else chr (ord c `mod` 256)) s)

foreign import prim "hs-read-file-bytes" primReadFileBytes :: FilePath -> IO String
foreign import prim "hs-write-file-bytes" primWriteFileBytes :: FilePath -> String -> IO ()
foreign import prim "hs-append-file-bytes" primAppendFileBytes :: FilePath -> String -> IO ()

encodeUtf8 :: String -> String
encodeUtf8 [] = []
encodeUtf8 (c : cs)
  | o < 0x80 = c : encodeUtf8 cs
  | o < 0x800 = byte (0xC0 + o `div` 64) : cont 1 (encodeUtf8 cs)
  | o < 0x10000 = byte (0xE0 + o `div` 4096) : cont 2 (cont 1 (encodeUtf8 cs))
  | otherwise = byte (0xF0 + o `div` 262144) : cont 3 (cont 2 (cont 1 (encodeUtf8 cs)))
  where
    o = ord c
    byte = chr
    cont k rest = chr (0x80 + (o `div` (64 ^ (k - 1))) `mod` 64) : rest

-- invalid bytes become U+FFFD
decodeUtf8 :: String -> String
decodeUtf8 [] = []
decodeUtf8 (c : cs)
  | o < 0x80 = c : decodeUtf8 cs
  | o >= 0xC0 && o < 0xE0 = multi 1 (o - 0xC0)
  | o >= 0xE0 && o < 0xF0 = multi 2 (o - 0xE0)
  | o >= 0xF0 && o < 0xF8 = multi 3 (o - 0xF0)
  | otherwise = '\xFFFD' : decodeUtf8 cs
  where
    o = ord c
    multi n start = case go n start cs of
      Just (v, rest) | v < 0x110000 -> chr v : decodeUtf8 rest
      _ -> '\xFFFD' : decodeUtf8 cs
    go 0 v rest = Just (v, rest)
    go k v (d : rest) | ord d >= 0x80 && ord d < 0xC0 = go (k - 1) (v * 64 + ord d - 0x80) rest
    go _ _ _ = Nothing

-- strict UTF-8 decoding: Nothing for invalid input (bad or missing
-- continuation bytes, overlong encodings, surrogates, values over 0x10FFFF)
decodeUtf8Strict :: String -> Maybe String
decodeUtf8Strict [] = Just []
decodeUtf8Strict (c : cs)
  | o < 0x80 = (c :) <$> decodeUtf8Strict cs
  | o >= 0xC0 && o < 0xE0 = multi 1 (o - 0xC0) 0x80
  | o >= 0xE0 && o < 0xF0 = multi 2 (o - 0xE0) 0x800
  | o >= 0xF0 && o < 0xF8 = multi 3 (o - 0xF0) 0x10000
  | otherwise = Nothing
  where
    o = ord c
    multi n start lo = case go n start cs of
      Just (v, rest) | v >= lo && v < 0x110000 && not (v >= 0xD800 && v < 0xE000) ->
        (chr v :) <$> decodeUtf8Strict rest
      _ -> Nothing
    go 0 v rest = Just (v, rest)
    go k v (d : rest) | ord d >= 0x80 && ord d < 0xC0 = go (k - 1) (v * 64 + ord d - 0x80) rest
    go _ _ _ = Nothing
