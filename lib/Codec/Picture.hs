-- Codec.Picture: images and PNG output, a small subset of the JuicyPixels
-- package.  Images are made with generateImage and saved with writePng
-- (uncompressed PNG).  Reading images is not supported.
module Codec.Picture (
  Image, imageWidth, imageHeight, generateImage, pixelAt,
  Pixel8, PixelRGB8(..), PixelRGBA8(..), Pixel,
  writePng, encodePng
  ) where

import Husk.Base
import Husk.ByteString (ByteString(..))
import qualified Data.ByteString.Lazy as BL

foreign import prim "hs-crc32" primCrc32 :: String -> Integer
foreign import prim "hs-adler32" primAdler32 :: String -> Integer

data Image a = Image { imageWidth :: Int, imageHeight :: Int, imagePixels :: Array (Int, Int) a }

type Pixel8 = Word8

data PixelRGB8 = PixelRGB8 Word8 Word8 Word8 deriving (Eq, Ord, Show)

data PixelRGBA8 = PixelRGBA8 Word8 Word8 Word8 Word8 deriving (Eq, Ord, Show)

-- pixel types that can be saved: their PNG color type and bytes
class Pixel a where
  pngColorType :: a -> Int
  pixelBytes :: a -> [Word8]

instance Pixel Word8 where
  pngColorType _ = 0
  pixelBytes w = [w]

instance Pixel PixelRGB8 where
  pngColorType _ = 2
  pixelBytes (PixelRGB8 r g b) = [r, g, b]

instance Pixel PixelRGBA8 where
  pngColorType _ = 6
  pixelBytes (PixelRGBA8 r g b a) = [r, g, b, a]

-- generateImage f width height: the pixel at (x, y) is f x y
generateImage :: Pixel a => (Int -> Int -> a) -> Int -> Int -> Image a
generateImage f w h = Image w h (listArray ((0, 0), (h - 1, w - 1)) [f x y | y <- [0 .. h - 1], x <- [0 .. w - 1]])

pixelAt :: Pixel a => Image a -> Int -> Int -> a
pixelAt img x y = imagePixels img ! (y, x)

writePng :: Pixel a => FilePath -> Image a -> IO ()
writePng path img = BL.writeFile path (encodePng img)

encodePng :: Pixel a => Image a -> BL.ByteString
encodePng img = BS png
  where
    w = imageWidth img
    h = imageHeight img
    ctype = pngColorType (pixelAt img 0 0)
    png = map chr [137, 80, 78, 71, 13, 10, 26, 10]
          ++ chunk "IHDR" (be32 (toInteger w) ++ be32 (toInteger h) ++ map chr [8, ctype, 0, 0, 0])
          ++ chunk "IDAT" (zlibStored raw)
          ++ chunk "IEND" []
    raw = concat ['\0' : concat [map byte (pixelBytes (pixelAt img x y)) | x <- [0 .. w - 1]] | y <- [0 .. h - 1]]
    byte b = chr (fromEnum b)

-- (bytes are Chars below 256)
chunk :: String -> String -> String
chunk name body = be32 (toInteger (length body)) ++ typeAndBody ++ be32 (primCrc32 typeAndBody)
  where typeAndBody = name ++ body

-- a zlib stream of uncompressed ("stored") deflate blocks
zlibStored :: String -> String
zlibStored bytes = "\x78\x01" ++ blocks bytes ++ be32 (primAdler32 bytes)
  where
    blocks bs = case splitAt 65535 bs of
      (b, []) -> block 1 b
      (b, rest) -> block 0 b ++ blocks rest
    block final b = let n = length b in chr final : le16 n ++ le16 (65535 - n) ++ b

be32 :: Integer -> String
be32 n = [chr (fromInteger ((n `div` (256 ^ k)) `mod` 256)) | k <- [3, 2, 1, 0 :: Int]]

le16 :: Int -> String
le16 n = [chr (n `mod` 256), chr (n `div` 256)]
