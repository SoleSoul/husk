-- Int wraps around at 64 bits; Int8 ... Word64 wrap at their sizes; Float is
-- single precision.  (Compare with GHC: the output is the same.)
import Data.Word
import Data.Int
import Data.Bits
import Numeric (showFFloat, showEFloat)
import Text.Printf
mainWords :: IO ()
mainWords = do
  print (255 + 1 :: Word8, 200 * 2 :: Word8, negate 1 :: Word8, 0 - 1 :: Word)
  print (127 + 1 :: Int8, minBound :: Int16, maxBound :: Word32, maxBound :: Word64, maxBound :: Word)
  print (fromIntegral (300 :: Int) :: Word8, fromIntegral (-1 :: Int) :: Word16, fromIntegral (200 :: Word8) :: Int8)
  print ([250 ..] :: [Word8])
  print (map fromEnum [minBound .. maxBound :: Int8] == [-128 .. 127])
  print (complement 0 :: Word8, shiftL 1 7 :: Int8, popCount (-1 :: Int32), testBit (5 :: Word8) 2, (0xF0 :: Word8) .&. 0x3C)
  print (divMod (-7) 2 :: (Int8, Int8), quotRem (-7) 2 :: (Int8, Int8), (200 :: Word8) `div` 3)
  print (read "42" :: Word8, show (3 :: Int32), toInteger (maxBound :: Word64) + 1)
  print (sum (map fromIntegral [1 .. 100 :: Int]) :: Word8)
  print (fromIntegral (maxBound :: Word) :: Int, fromIntegral (2^63 :: Integer) :: Int)
  print (maxBound :: Int, (maxBound :: Int) + 1, minBound `div` (2 :: Int), abs (minBound :: Int))
  print (product [1 .. 21 :: Int], 2 ^ 64 :: Int, 3 ^ 41 :: Int)
  print (shiftL 1 64 :: Int, shiftR (-8) 1 :: Int, popCount (-1 :: Int), testBit (-1 :: Int) 70)
circumference :: Float -> Float
circumference r = 2 * pi * r
circumference' :: Double -> Double
circumference' r = 2 * pi * r
mainFloats :: IO ()
mainFloats = do
  print (circumference 4.0, circumference' 4.0)
  print (0.1 + 0.2 :: Float, 1 / 3 :: Float, sqrt 2 :: Float, 2 / 3 * 100 :: Float)
  print [1.0, 1.5 .. 3 :: Float]
  print (realToFrac (0.1 :: Float) :: Double, realToFrac (0.1 :: Double) :: Float)
  print (fromIntegral (16777217 :: Int) :: Float, 1e10 :: Float, 1.0e-2 :: Float, 123456789 :: Float, 3.4e38 * 10 :: Float)
  print (truncate (3.7 :: Float) :: Int, round (2.5 :: Float) :: Int, floatDigits (1 :: Float), decodeFloat (0.1 :: Float))
  print (read "3.14159" :: Float, show (-2.5 :: Float), minimum [3.2, 1.1 :: Float])
  print (map (/ 7) [1, 2, 3 :: Float])
  putStrLn (showFFloat (Just 2) (3.14159 :: Float) "" ++ " " ++ showFFloat Nothing (0.1 :: Float) "" ++ " " ++ showEFloat Nothing (1234.5 :: Float) "")
  printf "%.3f %f\n" (2.0 / 3.0 :: Float) (0.1 :: Float)
  print (sum (replicate 10 0.1) :: Float, exp 1 :: Float, isNaN (0 / 0 :: Float))
  print (Just (-1.5 :: Float), [5.0e-2 :: Float, 0.1, 1.0e7, 9999999])

main :: IO ()
main = mainWords >> mainFloats
