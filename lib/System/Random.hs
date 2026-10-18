-- System.Random: pseudo-random numbers, with the interface of the random
-- package.  StdGen is SplitMix64, the generator random-1.2 uses, but the
-- numbers produced for a given seed are not guaranteed to match GHC's.
module System.Random (
  RandomGen(..), StdGen, mkStdGen, initStdGen,
  Random(..), uniform, uniformR,
  getStdGen, setStdGen, newStdGen, getStdRandom, randomIO, randomRIO
  ) where
import Husk.Base

foreign import prim "hs-system-time" primSystemTime :: IO Double

class RandomGen g where
  genWord64 :: g -> (Word64, g)
  split :: g -> (g, g)
  next :: g -> (Int, g)
  genRange :: g -> (Int, Int)
  next g = let (w, g') = genWord64 g in (fromIntegral w, g')
  genRange _ = (minBound, maxBound)

data StdGen = StdGen Word64 Word64   -- seed, gamma (odd)

instance Show StdGen where
  showsPrec d (StdGen s g) = showParen (d > 10)
    (showString "StdGen {unStdGen = SMGen " . shows s . showChar ' ' . shows g . showChar '}')

instance Eq StdGen where
  StdGen a b == StdGen c d = a == c && b == d

instance RandomGen StdGen where
  genWord64 (StdGen seed gamma) = let seed' = seed + gamma in (mix64 seed', StdGen seed' gamma)
  split (StdGen seed gamma) =
    let seed' = seed + gamma
        seed'' = seed' + gamma
    in (StdGen seed'' gamma, StdGen (mix64 seed') (mixGamma seed''))

mkStdGen :: Int -> StdGen
mkStdGen n = let s = fromIntegral n in StdGen (mix64 s) (mixGamma (s + goldenGamma))

-- a generator seeded from the clock
initStdGen :: IO StdGen
initStdGen = do
  t <- primSystemTime
  c <- getCPUTime
  return (mkStdGen (truncate (t * 1000000) `xor` fromInteger c))

-- SplitMix64 (Word64 arithmetic wraps around)
shiftXor :: Int -> Word64 -> Word64
shiftXor n w = w `xor` (w `shiftR` n)

goldenGamma :: Word64
goldenGamma = 0x9e3779b97f4a7c15

mix64 :: Word64 -> Word64
mix64 z = shiftXor 33 (shiftXor 33 (shiftXor 33 z * 0xff51afd7ed558ccd) * 0xc4ceb9fe1a85ec53)

mix64variant13 :: Word64 -> Word64
mix64variant13 z = shiftXor 31 (shiftXor 27 (shiftXor 30 z * 0xbf58476d1ce4e5b9) * 0x94d049bb133111eb)

mixGamma :: Word64 -> Word64
mixGamma z =
  let z' = mix64variant13 z .|. 1
      n = popCount (z' `xor` (z' `shiftR` 1))
  in if n >= 24 then z' else z' `xor` 0xaaaaaaaaaaaaaaaa

-- uniform in [0, r]: take enough 64-bit words, mask, and reject values above r
uniformUpTo :: RandomGen g => Integer -> g -> (Integer, g)
uniformUpTo r g0 = loop g0
  where
    bits = length (takeWhile (> 0) (iterate (`div` 2) r))
    nwords = max 1 ((bits + 63) `div` 64)
    mask = 2 ^ bits - 1
    loop g = let (x, g') = word nwords 0 g
                 x' = x .&. mask
             in if x' > r then loop g' else (x', g')
    word 0 acc g = (acc, g)
    word k acc g = let (w, g') = genWord64 g in word (k - 1) (acc * 18446744073709551616 + toInteger w) g'

-- uniform in [0, 1]
uniformDouble01 :: RandomGen g => g -> (Double, g)
uniformDouble01 g = let (w, g') = genWord64 g in (fromIntegral w / 18446744073709551615, g')

class Random a where
  randomR :: RandomGen g => (a, a) -> g -> (a, g)
  random :: RandomGen g => g -> (a, g)
  randomRs :: RandomGen g => (a, a) -> g -> [a]
  randoms :: RandomGen g => g -> [a]
  randomRs range g = let (x, g') = randomR range g in x : randomRs range g'
  randoms g = let (x, g') = random g in x : randoms g'

integralR :: (Integral a, RandomGen g) => (a, a) -> g -> (a, g)
integralR (lo, hi) g
  | lo > hi = integralR (hi, lo) g
  | otherwise = let (x, g') = uniformUpTo (toInteger hi - toInteger lo) g
                in (fromInteger (toInteger lo + x), g')

instance Random Int where
  randomR = integralR
  random g = let (w, g') = genWord64 g in (fromIntegral w, g')

instance Random Integer where
  randomR = integralR
  random g = let (w, g') = genWord64 g in (toInteger (fromIntegral w :: Int), g')

instance Random Word8 where { randomR = integralR; random = randomR (minBound, maxBound) }
instance Random Word where { randomR = integralR; random = randomR (minBound, maxBound) }

instance Random Double where
  randomR (l, h) g
    | l == h = (l, g)
    | otherwise = let (x, g') = uniformDouble01 g in (x * l + (1 - x) * h, g')
  random = randomR (0, 1)

instance Random Float where
  randomR (l, h) g = let (x, g') = randomR (realToFrac l :: Double, realToFrac h) g in (realToFrac x, g')
  random = randomR (0, 1)

instance Random Bool where
  randomR (a, b) g = let (x, g') = integralR (fromEnum a, fromEnum b) g in (toEnum x, g')
  random = randomR (False, True)

instance Random Char where
  randomR (a, b) g = let (x, g') = integralR (fromEnum a, fromEnum b) g in (toEnum x, g')
  random = randomR (minBound, maxBound)

instance (Random a, Random b) => Random (a, b) where
  randomR ((a1, b1), (a2, b2)) g =
    let (a, g1) = randomR (a1, a2) g
        (b, g2) = randomR (b1, b2) g1
    in ((a, b), g2)
  random g = let (a, g1) = random g
                 (b, g2) = random g1
             in ((a, b), g2)

-- random-1.2's names for the same operations
uniform :: (RandomGen g, Random a) => g -> (a, g)
uniform = random

uniformR :: (RandomGen g, Random a) => (a, a) -> g -> (a, g)
uniformR = randomR

-- the global generator
theStdGen :: IORef StdGen
theStdGen = unsafePerformIO (initStdGen >>= newIORef)

getStdGen :: IO StdGen
getStdGen = readIORef theStdGen

setStdGen :: StdGen -> IO ()
setStdGen = writeIORef theStdGen

-- splits the global generator: one half replaces it, the other is returned
newStdGen :: IO StdGen
newStdGen = do
  g <- readIORef theStdGen
  let (a, b) = split g
  writeIORef theStdGen a
  return b

getStdRandom :: (StdGen -> (a, StdGen)) -> IO a
getStdRandom f = do
  g <- readIORef theStdGen
  let (x, g') = f g
  writeIORef theStdGen g'
  return x

randomIO :: Random a => IO a
randomIO = getStdRandom random

randomRIO :: Random a => (a, a) -> IO a
randomRIO range = getStdRandom (randomR range)
