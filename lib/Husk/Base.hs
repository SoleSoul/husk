-- Husk.Base: the core of the husk standard library.
--
-- Everything the Prelude and the other library modules export is defined
-- here (much like GHC's base package).  Primitive operations are imported
-- from Scheme with
--     foreign import prim "scheme-procedure" name :: Type   -- strict args
--     foreign import lazy "scheme-procedure" name :: Type   -- lazy args
-- and primitive types are declared as data types without constructors.
module Husk.Base where

infixr 9  .
infixr 8  ^, ^^, **
infixl 7  *, /, `quot`, `rem`, `div`, `mod`, %
infixl 6  +, -
infixr 6  <>
infixr 5  ++
infix  4  ==, /=, <, <=, >=, >, `elem`, `notElem`
infixl 4  <$>, <$, $>, <*>, *>, <*, <**>
infixr 3  &&
infixr 2  ||
infixl 3  <|>
infixl 1  >>, >>=, &, <&>
infixr 1  =<<, >=>, <=<
infixr 0  $, $!, `seq`
infixl 9  !!
infixl 0  `on`
infixl 9  !, //
infixl 8  `shiftL`, `shiftR`, `shift`
infixl 7  .&.
infixl 6  `xor`
infixl 5  .|.

------------------------------------------------------------------------
-- Primitive types

data Int
data Integer
data Double
data Char
data IO a
data IORef a
data Ratio a
data Handle
data Vector a
data SomeException

type String = [Char]
type Rational = Ratio Integer
type ShowS = String -> String
type ReadS a = String -> [(a, String)]
type FilePath = String
type IOError = IOException

data Bool = False | True deriving (Eq, Ord, Show, Read, Bounded)
data Ordering = LT | EQ | GT deriving (Eq, Ord, Show, Read, Enum, Bounded)
data Maybe a = Nothing | Just a deriving (Eq, Ord, Show, Read)
data Either a b = Left a | Right b deriving (Eq, Ord, Show, Read)

------------------------------------------------------------------------
-- Primitives

foreign import prim "=" primIntEq :: Int -> Int -> Bool
foreign import prim "<" primIntLt :: Int -> Int -> Bool
foreign import prim "<=" primIntLe :: Int -> Int -> Bool
foreign import prim ">" primIntGt :: Int -> Int -> Bool
foreign import prim ">=" primIntGe :: Int -> Int -> Bool
foreign import prim "hs-compare" primCompareInt :: Int -> Int -> Ordering
foreign import prim "hs-int-add" primIntAdd :: Int -> Int -> Int
foreign import prim "hs-int-sub" primIntSub :: Int -> Int -> Int
foreign import prim "hs-int-mul" primIntMul :: Int -> Int -> Int
foreign import prim "hs-int-negate" primIntNegate :: Int -> Int
foreign import prim "hs-int-abs" primIntAbs :: Int -> Int
foreign import prim "hs-signum" primIntSignum :: Int -> Int
foreign import prim "hs-quot" primIntQuot :: Int -> Int -> Int
foreign import prim "hs-rem" primIntRem :: Int -> Int -> Int
foreign import prim "hs-div" primIntDiv :: Int -> Int -> Int
foreign import prim "hs-mod" primIntMod :: Int -> Int -> Int
foreign import prim "hs-quot-rem" primIntQuotRem :: Int -> Int -> (Int, Int)
foreign import prim "hs-div-mod" primIntDivMod :: Int -> Int -> (Int, Int)
foreign import prim "max" primIntMax :: Int -> Int -> Int
foreign import prim "min" primIntMin :: Int -> Int -> Int
foreign import prim "hs-show-int" primShowInt :: Int -> String
foreign import prim "hs-int64" primIntegerToInt :: Integer -> Int
foreign import prim "hs-unsafe-coerce" primIntToInteger :: Int -> Integer
foreign import prim "inexact" primIntToDouble :: Int -> Double

foreign import prim "=" primIntegerEq :: Integer -> Integer -> Bool
foreign import prim "<" primIntegerLt :: Integer -> Integer -> Bool
foreign import prim "<=" primIntegerLe :: Integer -> Integer -> Bool
foreign import prim ">" primIntegerGt :: Integer -> Integer -> Bool
foreign import prim ">=" primIntegerGe :: Integer -> Integer -> Bool
foreign import prim "hs-compare" primCompareInteger :: Integer -> Integer -> Ordering
foreign import prim "+" primIntegerAdd :: Integer -> Integer -> Integer
foreign import prim "-" primIntegerSub :: Integer -> Integer -> Integer
foreign import prim "*" primIntegerMul :: Integer -> Integer -> Integer
foreign import prim "-" primIntegerNegate :: Integer -> Integer
foreign import prim "abs" primIntegerAbs :: Integer -> Integer
foreign import prim "hs-signum" primIntegerSignum :: Integer -> Integer
foreign import prim "hs-quot" primIntegerQuot :: Integer -> Integer -> Integer
foreign import prim "hs-rem" primIntegerRem :: Integer -> Integer -> Integer
foreign import prim "hs-div" primIntegerDiv :: Integer -> Integer -> Integer
foreign import prim "hs-mod" primIntegerMod :: Integer -> Integer -> Integer
foreign import prim "hs-quot-rem" primIntegerQuotRem :: Integer -> Integer -> (Integer, Integer)
foreign import prim "hs-div-mod" primIntegerDivMod :: Integer -> Integer -> (Integer, Integer)
foreign import prim "max" primIntegerMax :: Integer -> Integer -> Integer
foreign import prim "min" primIntegerMin :: Integer -> Integer -> Integer
foreign import prim "hs-show-int" primShowInteger :: Integer -> String
foreign import prim "inexact" primIntegerToDouble :: Integer -> Double
foreign import prim "hs-gcd" primIntegerGcd :: Integer -> Integer -> Integer
foreign import prim "hs-lcm" primIntegerLcm :: Integer -> Integer -> Integer
foreign import prim "hs-int-pow" primIntegerPow :: Integer -> Integer -> Integer

foreign import prim "fl=" primDoubleEq :: Double -> Double -> Bool
foreign import prim "fl<" primDoubleLt :: Double -> Double -> Bool
foreign import prim "fl<=" primDoubleLe :: Double -> Double -> Bool
foreign import prim "fl>" primDoubleGt :: Double -> Double -> Bool
foreign import prim "fl>=" primDoubleGe :: Double -> Double -> Bool
foreign import prim "fl+" primDoubleAdd :: Double -> Double -> Double
foreign import prim "fl-" primDoubleSub :: Double -> Double -> Double
foreign import prim "fl*" primDoubleMul :: Double -> Double -> Double
foreign import prim "fl/" primDoubleDiv :: Double -> Double -> Double
foreign import prim "fl-" primDoubleNegate :: Double -> Double
foreign import prim "flabs" primDoubleAbs :: Double -> Double
foreign import prim "hs-signum" primDoubleSignum :: Double -> Double
foreign import prim "flmax" primDoubleMax :: Double -> Double -> Double
foreign import prim "flmin" primDoubleMin :: Double -> Double -> Double
foreign import prim "hs-show-double" primShowDouble :: Double -> String
foreign import prim "hs-truncate" primDoubleTruncate :: Double -> Integer
foreign import prim "hs-round" primDoubleRound :: Double -> Integer
foreign import prim "hs-ceiling" primDoubleCeiling :: Double -> Integer
foreign import prim "hs-floor" primDoubleFloor :: Double -> Integer
foreign import prim "exact" primDoubleToRational :: Double -> Rational
foreign import prim "inexact" primRationalToDouble :: Rational -> Double
foreign import prim "hs-sqrt" primSqrt :: Double -> Double
foreign import prim "hs-exp" primExp :: Double -> Double
foreign import prim "hs-log" primLog :: Double -> Double
foreign import prim "hs-sin" primSin :: Double -> Double
foreign import prim "hs-cos" primCos :: Double -> Double
foreign import prim "hs-tan" primTan :: Double -> Double
foreign import prim "hs-asin" primAsin :: Double -> Double
foreign import prim "hs-acos" primAcos :: Double -> Double
foreign import prim "hs-atan" primAtan :: Double -> Double
foreign import prim "hs-sinh" primSinh :: Double -> Double
foreign import prim "hs-cosh" primCosh :: Double -> Double
foreign import prim "hs-tanh" primTanh :: Double -> Double
foreign import prim "hs-asinh" primAsinh :: Double -> Double
foreign import prim "hs-acosh" primAcosh :: Double -> Double
foreign import prim "hs-atanh" primAtanh :: Double -> Double
foreign import prim "hs-pow-double" primPowDouble :: Double -> Double -> Double
foreign import prim "hs-atan2" primAtan2 :: Double -> Double -> Double
foreign import prim "hs-is-nan" primIsNaN :: Double -> Bool
foreign import prim "hs-is-infinite" primIsInfinite :: Double -> Bool
foreign import prim "hs-decode-double" primDecodeDouble :: Double -> (Integer, Int)

foreign import prim "=" primRatioEq :: Ratio a -> Ratio a -> Bool
foreign import prim "<" primRatioLt :: Ratio a -> Ratio a -> Bool
foreign import prim "<=" primRatioLe :: Ratio a -> Ratio a -> Bool
foreign import prim "hs-compare" primCompareRatio :: Ratio a -> Ratio a -> Ordering
foreign import prim "+" primRatioAdd :: Ratio a -> Ratio a -> Ratio a
foreign import prim "-" primRatioSub :: Ratio a -> Ratio a -> Ratio a
foreign import prim "*" primRatioMul :: Ratio a -> Ratio a -> Ratio a
foreign import prim "/" primRatioDiv :: Ratio a -> Ratio a -> Ratio a
foreign import prim "-" primRatioNegate :: Ratio a -> Ratio a
foreign import prim "abs" primRatioAbs :: Ratio a -> Ratio a
foreign import prim "hs-signum" primRatioSignum :: Ratio a -> Ratio a
foreign import prim "hs-make-ratio" primMakeRatio :: Integer -> Integer -> Ratio a
foreign import prim "hs-numerator" primNumerator :: Ratio a -> Integer
foreign import prim "hs-denominator" primDenominator :: Ratio a -> Integer
foreign import prim "hs-truncate" primRatioTruncate :: Ratio a -> Integer
foreign import prim "hs-round" primRatioRound :: Ratio a -> Integer
foreign import prim "hs-ceiling" primRatioCeiling :: Ratio a -> Integer
foreign import prim "hs-floor" primRatioFloor :: Ratio a -> Integer
foreign import prim "hs-unsafe-coerce" primIntegerToRatio :: Integer -> Ratio a
foreign import prim "hs-unsafe-coerce" primRationalToRatio :: Rational -> Ratio a
foreign import prim "hs-unsafe-coerce" primRatioToRational :: Ratio a -> Rational

foreign import prim "char=?" primCharEq :: Char -> Char -> Bool
foreign import prim "char<?" primCharLt :: Char -> Char -> Bool
foreign import prim "char<=?" primCharLe :: Char -> Char -> Bool
foreign import prim "char>?" primCharGt :: Char -> Char -> Bool
foreign import prim "char>=?" primCharGe :: Char -> Char -> Bool
foreign import prim "char->integer" ord :: Char -> Int
foreign import prim "hs-int-to-char" chr :: Int -> Char
foreign import prim "hs-show-char" primShowChar :: Char -> String
foreign import prim "hs-show-string" primShowString :: String -> String
foreign import prim "char-upcase" toUpper :: Char -> Char
foreign import prim "char-downcase" toLower :: Char -> Char
foreign import prim "char-alphabetic?" isAlpha :: Char -> Bool
foreign import prim "char-alphabetic?" isLetter :: Char -> Bool
foreign import prim "char-upper-case?" isUpper :: Char -> Bool
foreign import prim "char-lower-case?" isLower :: Char -> Bool
foreign import prim "hs-is-space" isSpace :: Char -> Bool
foreign import prim "hs-is-punctuation" isPunctuation :: Char -> Bool
foreign import prim "hs-is-symbol" isSymbol :: Char -> Bool

foreign import prim "hs-tag-of" primTagOf :: a -> Int
foreign import prim "hs-unsafe-coerce" primUnsafeCoerce :: a -> b
foreign import prim "hs-to-enum" primToEnum :: String -> Int -> Int -> a
foreign import lazy "hs-seq" seq :: a -> b -> b
foreign import prim "hs-error-hs" error :: [Char] -> a
foreign import prim "hs-error-hs" errorWithoutStackTrace :: [Char] -> a
foreign import prim "hs-undefined" undefined :: a
-- the compiler calls these for `error` and `undefined` in user code, with
-- the call site ("error, called at prog.hs:3:9 in main:Main")
foreign import prim "hs-error-at" primErrorAt :: String -> [Char] -> a
foreign import prim "hs-undefined-at" primUndefinedAt :: String -> a
foreign import prim "hs-error-call-message" primErrorCallMessage :: SomeException -> String
foreign import lazy "hs-trace" primTrace :: String -> a -> a
foreign import prim "hs-reads-integer" primReadsInteger :: String -> [(Integer, String)]
foreign import prim "hs-reads-double" primReadsDouble :: String -> [(Double, String)]
foreign import prim "hs-reads-char" primReadsChar :: String -> [(Char, String)]
foreign import prim "hs-reads-string" primReadsString :: String -> [(String, String)]
foreign import prim "hs-show-ffloat" primShowFFloat :: Int -> Bool -> Double -> String
foreign import prim "hs-show-efloat" primShowEFloat :: Int -> Bool -> Double -> String
foreign import prim "hs-float32" primFloat32 :: Double -> Double
foreign import prim "hs-show-float" primShowFloat :: Double -> String
foreign import prim "hs-show-hex" primShowHex :: Integer -> String
foreign import prim "hs-show-oct" primShowOct :: Integer -> String

foreign import prim "hs-shift-l" primShiftL :: Integer -> Int -> Integer
foreign import prim "hs-shift-r" primShiftR :: Integer -> Int -> Integer
foreign import prim "hs-test-bit" primTestBit :: Integer -> Int -> Bool
foreign import prim "hs-pop-count" primPopCount :: Integer -> Int
foreign import prim "hs-int-pop-count" primIntPopCount :: Int -> Int
foreign import prim "hs-int-shift-l" primIntShiftL :: Int -> Int -> Int
foreign import prim "hs-wrap-bits" primWrapBits :: Int -> Bool -> Integer -> Integer
foreign import prim "hs-and" primAnd :: Integer -> Integer -> Integer
foreign import prim "hs-or" primOr :: Integer -> Integer -> Integer
foreign import prim "hs-xor" primXor :: Integer -> Integer -> Integer
foreign import prim "hs-complement" primComplement :: Integer -> Integer

foreign import lazy "hs-io-bind" primBindIO :: IO a -> (a -> IO b) -> IO b
foreign import lazy "hs-io-then" primThenIO :: IO a -> IO b -> IO b
foreign import lazy "hs-io-return" primReturnIO :: a -> IO a
foreign import prim "hs-put-str" putStr :: String -> IO ()
foreign import prim "hs-put-char" putChar :: Char -> IO ()
foreign import prim "hs-get-line" getLine :: IO String
foreign import prim "hs-get-char" getChar :: IO Char
foreign import prim "hs-get-contents" getContents :: IO String
foreign import prim "hs-is-eof" isEOF :: IO Bool
foreign import prim "hs-read-file" readFile :: FilePath -> IO String
foreign import prim "hs-write-file" writeFile :: FilePath -> String -> IO ()
foreign import prim "hs-append-file" appendFile :: FilePath -> String -> IO ()
foreign import prim "hs-stdout" stdout :: Handle
foreign import prim "hs-stderr" stderr :: Handle
foreign import prim "hs-stdin" stdin :: Handle
foreign import prim "hs-hput-str" hPutStr :: Handle -> String -> IO ()
foreign import prim "hs-hget-line" hGetLine :: Handle -> IO String
foreign import prim "hs-hget-contents" hGetContents :: Handle -> IO String
foreign import prim "hs-hflush" hFlush :: Handle -> IO ()
foreign import prim "hs-hclose" hClose :: Handle -> IO ()
foreign import prim "hs-his-eof" hIsEOF :: Handle -> IO Bool
foreign import prim "hs-open-file" primOpenFile :: FilePath -> Int -> IO Handle
foreign import prim "hs-exit-with" primExitWith :: Int -> IO a
foreign import prim "hs-get-args" getArgs :: IO [String]
foreign import prim "hs-get-prog-name" getProgName :: IO String
foreign import prim "hs-get-env" lookupEnv :: String -> IO (Maybe String)
foreign import prim "hs-cpu-time" getCPUTime :: IO Integer
foreign import prim "hs-new-ioref" newIORef :: a -> IO (IORef a)
foreign import prim "hs-read-ioref" readIORef :: IORef a -> IO a
foreign import prim "hs-write-ioref" writeIORef :: IORef a -> a -> IO ()
foreign import prim "hs-catch" primCatch :: IO a -> (SomeException -> IO a) -> IO a
foreign import lazy "hs-evaluate" evaluate :: a -> IO a
foreign import prim "hs-show-exception" primShowException :: SomeException -> String
foreign import prim "hs-exception-kind" primExceptionKind :: SomeException -> Int
foreign import prim "hs-exception-match" primExceptionMatch :: String -> SomeException -> Maybe a
foreign import lazy "hs-make-typed" primMakeTyped :: String -> String -> a -> SomeException
foreign import prim "hs-make-error-call" primMakeErrorCall :: String -> SomeException
foreign import prim "hs-make-io-error" primMakeIOError :: String -> SomeException
foreign import prim "hs-make-arith" primMakeArith :: String -> SomeException
foreign import prim "hs-raise" primRaise :: SomeException -> a
foreign import prim "hs-throw-io" primRaiseIO :: SomeException -> IO a
foreign import prim "hs-unsafe-perform-io" unsafePerformIO :: IO a -> a
foreign import prim "hs-io-fix" fixIO :: (a -> IO a) -> IO a

foreign import prim "hs-array-from-list" primVectorFromList :: Int -> [a] -> Vector a
foreign import prim "hs-array-ref" primVectorRef :: Vector a -> Int -> a
foreign import prim "hs-array-length" primVectorLength :: Vector a -> Int
foreign import prim "hs-array-elems" primVectorElems :: Vector a -> [a]
foreign import prim "hs-array-update" primVectorUpdate :: Vector a -> [(Int, a)] -> Vector a
foreign import prim "hs-array-accum" primVectorAccum :: (e -> a -> e) -> Vector e -> [(Int, a)] -> Vector e

------------------------------------------------------------------------
-- Basic combinators

id :: a -> a
id x = x

const :: a -> b -> a
const x _ = x

(.) :: (b -> c) -> (a -> b) -> a -> c
(.) f g = \x -> f (g x)

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

($) :: (a -> b) -> a -> b
f $ x = f x

($!) :: (a -> b) -> a -> b
f $! x = x `seq` f x

(&) :: a -> (a -> b) -> b
x & f = f x

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on op f x y = f x `op` f y

fix :: (a -> a) -> a
fix f = let x = f x in x

asTypeOf :: a -> a -> a
asTypeOf x _ = x

until :: (a -> Bool) -> (a -> a) -> a -> a
until p f x = if p x then x else until p f (f x)

otherwise :: Bool
otherwise = True

not :: Bool -> Bool
not True = False
not False = True

(&&) :: Bool -> Bool -> Bool
True && x = x
False && _ = False

(||) :: Bool -> Bool -> Bool
True || _ = True
False || x = x

bool :: a -> a -> Bool -> a
bool f t b = if b then t else f

fst :: (a, b) -> a
fst (x, _) = x

snd :: (a, b) -> b
snd (_, y) = y

curry :: ((a, b) -> c) -> a -> b -> c
curry f x y = f (x, y)

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f p = f (fst p) (snd p)

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

maybe :: b -> (a -> b) -> Maybe a -> b
maybe n _ Nothing = n
maybe _ f (Just x) = f x

either :: (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left x) = f x
either _ g (Right y) = g y

primThenCmp :: Ordering -> Ordering -> Ordering
primThenCmp EQ y = y
primThenCmp x _ = x

------------------------------------------------------------------------
-- Eq and Ord

class Eq a where
  (==), (/=) :: a -> a -> Bool
  x /= y = not (x == y)
  x == y = not (x /= y)

class Eq a => Ord a where
  compare :: a -> a -> Ordering
  (<), (<=), (>), (>=) :: a -> a -> Bool
  max, min :: a -> a -> a
  compare x y = if x == y then EQ else if x <= y then LT else GT
  x < y = case compare x y of { LT -> True; _ -> False }
  x <= y = case compare x y of { GT -> False; _ -> True }
  x > y = case compare x y of { GT -> True; _ -> False }
  x >= y = case compare x y of { LT -> False; _ -> True }
  max x y = if x <= y then y else x
  min x y = if x <= y then x else y

comparing :: Ord b => (a -> b) -> a -> a -> Ordering
comparing f x y = compare (f x) (f y)

newtype Down a = Down a deriving (Eq, Show)

getDown :: Down a -> a
getDown (Down a) = a

instance Ord a => Ord (Down a) where
  compare (Down x) (Down y) = compare y x

instance Eq Int where
  (==) = primIntEq
  x /= y = not (primIntEq x y)

instance Ord Int where
  compare = primCompareInt
  (<) = primIntLt
  (<=) = primIntLe
  (>) = primIntGt
  (>=) = primIntGe
  max = primIntMax
  min = primIntMin

instance Eq Integer where
  (==) = primIntegerEq
  x /= y = not (primIntegerEq x y)

instance Ord Integer where
  compare = primCompareInteger
  (<) = primIntegerLt
  (<=) = primIntegerLe
  (>) = primIntegerGt
  (>=) = primIntegerGe
  max = primIntegerMax
  min = primIntegerMin

instance Eq Double where
  (==) = primDoubleEq
  x /= y = not (primDoubleEq x y)

instance Ord Double where
  compare x y = if primDoubleLt x y then LT else if primDoubleEq x y then EQ else GT
  (<) = primDoubleLt
  (<=) = primDoubleLe
  (>) = primDoubleGt
  (>=) = primDoubleGe
  max = primDoubleMax
  min = primDoubleMin

instance Eq Char where
  (==) = primCharEq
  x /= y = not (primCharEq x y)

instance Ord Char where
  compare x y = primCompareInt (ord x) (ord y)
  (<) = primCharLt
  (<=) = primCharLe
  (>) = primCharGt
  (>=) = primCharGe

instance Eq () where
  () == () = True

instance Ord () where
  compare () () = EQ

instance Eq a => Eq [a] where
  [] == [] = True
  (x:xs) == (y:ys) = x == y && xs == ys
  _ == _ = False

instance Ord a => Ord [a] where
  compare [] [] = EQ
  compare [] (_:_) = LT
  compare (_:_) [] = GT
  compare (x:xs) (y:ys) = case compare x y of
    EQ -> compare xs ys
    other -> other

instance (Eq a, Eq b) => Eq (a, b) where
  (a, b) == (c, d) = a == c && b == d

instance (Ord a, Ord b) => Ord (a, b) where
  compare (a, b) (c, d) = primThenCmp (compare a c) (compare b d)

instance (Eq a, Eq b, Eq c) => Eq (a, b, c) where
  (a, b, c) == (d, e, f) = a == d && b == e && c == f

instance (Ord a, Ord b, Ord c) => Ord (a, b, c) where
  compare (a, b, c) (d, e, f) = primThenCmp (compare a d) (primThenCmp (compare b e) (compare c f))

instance (Eq a, Eq b, Eq c, Eq d) => Eq (a, b, c, d) where
  (a, b, c, d) == (e, f, g, h) = a == e && b == f && c == g && d == h

instance (Ord a, Ord b, Ord c, Ord d) => Ord (a, b, c, d) where
  compare (a, b, c, d) (e, f, g, h) =
    primThenCmp (compare a e) (primThenCmp (compare b f) (primThenCmp (compare c g) (compare d h)))

instance (Eq a, Eq b, Eq c, Eq d, Eq e) => Eq (a, b, c, d, e) where
  (a, b, c, d, e) == (a', b', c', d', e') = a == a' && b == b' && c == c' && d == d' && e == e'

instance (Ord a, Ord b, Ord c, Ord d, Ord e) => Ord (a, b, c, d, e) where
  compare (a, b, c, d, e) (a', b', c', d', e') =
    primThenCmp (compare a a') (primThenCmp (compare b b') (primThenCmp (compare c c')
      (primThenCmp (compare d d') (compare e e'))))

instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f) => Eq (a, b, c, d, e, f) where
  (a, b, c, d, e, f) == (a', b', c', d', e', f') = a == a' && b == b' && c == c' && d == d' && e == e' && f == f'

instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f) => Ord (a, b, c, d, e, f) where
  compare (a, b, c, d, e, f) (a', b', c', d', e', f') =
    primThenCmp (compare a a') (primThenCmp (compare b b') (primThenCmp (compare c c')
      (primThenCmp (compare d d') (primThenCmp (compare e e') (compare f f')))))

instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g) => Eq (a, b, c, d, e, f, g) where
  (a, b, c, d, e, f, g) == (a', b', c', d', e', f', g') =
    a == a' && b == b' && c == c' && d == d' && e == e' && f == f' && g == g'

instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g) => Ord (a, b, c, d, e, f, g) where
  compare (a, b, c, d, e, f, g) (a', b', c', d', e', f', g') =
    primThenCmp (compare a a') (primThenCmp (compare b b') (primThenCmp (compare c c')
      (primThenCmp (compare d d') (primThenCmp (compare e e') (primThenCmp (compare f f') (compare g g'))))))

------------------------------------------------------------------------
-- Enum and Bounded

class Enum a where
  succ, pred :: a -> a
  toEnum :: Int -> a
  fromEnum :: a -> Int
  enumFrom :: a -> [a]
  enumFromThen :: a -> a -> [a]
  enumFromTo :: a -> a -> [a]
  enumFromThenTo :: a -> a -> a -> [a]
  succ x = toEnum (fromEnum x + 1)
  pred x = toEnum (fromEnum x - 1)
  enumFrom x = map toEnum (enumFrom (fromEnum x))
  enumFromThen x y = map toEnum (enumFromThen (fromEnum x) (fromEnum y))
  enumFromTo x y = map toEnum (enumFromTo (fromEnum x) (fromEnum y))
  enumFromThenTo x y z = map toEnum (enumFromThenTo (fromEnum x) (fromEnum y) (fromEnum z))

class Bounded a where
  minBound, maxBound :: a

instance Enum Int where
  succ x = if x == maxBound then error "Prelude.Enum.succ{Int}: tried to take `succ' of maxBound" else x + 1
  pred x = if x == minBound then error "Prelude.Enum.pred{Int}: tried to take `pred' of minBound" else x - 1
  toEnum x = x
  fromEnum x = x
  enumFrom x = enumFromTo x maxBound
  enumFromTo x y = if x > y then [] else x : (if x == y then [] else enumFromTo (x + 1) y)
  enumFromThen x y = enumFromThenTo x y (if y >= x then maxBound else minBound)
  enumFromThenTo x y z = intEnumFromThenTo (toInteger x) (toInteger y - toInteger x) (toInteger z)

-- (in Integer, so that the last step can't wrap around)
intEnumFromThenTo :: Integer -> Integer -> Integer -> [Int]
intEnumFromThenTo x d z
  | d >= 0 = if x > z then [] else primIntegerToInt x : intEnumFromThenTo (x + d) d z
  | otherwise = if x < z then [] else primIntegerToInt x : intEnumFromThenTo (x + d) d z

instance Bounded Int where
  minBound = -9223372036854775808
  maxBound = 9223372036854775807

instance Enum Integer where
  succ x = x + 1
  pred x = x - 1
  toEnum x = primIntToInteger x
  fromEnum x = primIntegerToInt x
  enumFrom x = x : enumFrom (x + 1)
  enumFromTo x y = if x > y then [] else x : enumFromTo (x + 1) y
  enumFromThen x y = x : enumFromThen y (y + y - x)
  enumFromThenTo x y z = integerEnumFromThenTo x (y - x) z

integerEnumFromThenTo :: Integer -> Integer -> Integer -> [Integer]
integerEnumFromThenTo x d z
  | d >= 0 = if x > z then [] else x : integerEnumFromThenTo (x + d) d z
  | otherwise = if x < z then [] else x : integerEnumFromThenTo (x + d) d z

instance Enum Char where
  toEnum = chr
  fromEnum = ord
  enumFrom c = map chr (enumFromTo (ord c) 1114111)
  enumFromThen c d = map chr (enumFromThenTo (ord c) (ord d) (if d >= c then 1114111 else 0))

instance Bounded Char where
  minBound = '\0'
  maxBound = '\1114111'

instance Enum Double where
  succ x = x + 1
  pred x = x - 1
  toEnum = primIntToDouble
  fromEnum x = primIntegerToInt (primDoubleTruncate x)
  enumFrom = numericEnumFrom
  enumFromThen = numericEnumFromThen
  enumFromTo = numericEnumFromTo
  enumFromThenTo = numericEnumFromThenTo

numericEnumFrom :: Fractional a => a -> [a]
numericEnumFrom n = n : numericEnumFrom (n + 1)

numericEnumFromThen :: Fractional a => a -> a -> [a]
numericEnumFromThen n m = n : numericEnumFromThen m (m + m - n)

numericEnumFromTo :: (Ord a, Fractional a) => a -> a -> [a]
numericEnumFromTo n m = takeWhile (<= m + 1 / 2) (numericEnumFrom n)

numericEnumFromThenTo :: (Ord a, Fractional a) => a -> a -> a -> [a]
numericEnumFromThenTo e1 e2 e3 = takeWhile p (numericEnumFromThen e1 e2)
  where mid = (e2 - e1) / 2
        p x = if e2 >= e1 then x <= e3 + mid else x >= e3 + mid

instance Enum Bool where
  fromEnum False = 0
  fromEnum True = 1
  toEnum 0 = False
  toEnum 1 = True
  toEnum n = error ("Prelude.Enum.Bool.toEnum: bad argument")
  enumFrom x = enumFromTo x True
  enumFromThen x y = enumFromThenTo x y (if fromEnum y >= fromEnum x then True else False)

instance Enum () where
  fromEnum _ = 0
  toEnum _ = ()
  enumFrom _ = [()]
  enumFromThen _ _ = repeat ()

instance Bounded () where
  minBound = ()
  maxBound = ()

instance (Bounded a, Bounded b) => Bounded (a, b) where
  minBound = (minBound, minBound)
  maxBound = (maxBound, maxBound)

------------------------------------------------------------------------
-- Numbers

class Num a where
  (+), (-), (*) :: a -> a -> a
  negate, abs, signum :: a -> a
  fromInteger :: Integer -> a
  x - y = x + negate y
  negate x = fromInteger 0 - x

class (Num a, Ord a) => Real a where
  toRational :: a -> Rational

class (Real a, Enum a) => Integral a where
  quot, rem, div, mod :: a -> a -> a
  quotRem, divMod :: a -> a -> (a, a)
  toInteger :: a -> Integer
  n `quot` d = fst (quotRem n d)
  n `rem` d = snd (quotRem n d)
  n `div` d = fst (divMod n d)
  n `mod` d = snd (divMod n d)
  divMod n d = (div n d, mod n d)
  quotRem n d = (quot n d, rem n d)

class Num a => Fractional a where
  (/) :: a -> a -> a
  recip :: a -> a
  fromRational :: Rational -> a
  recip x = 1 / x
  x / y = x * recip y

class Fractional a => Floating a where
  pi :: a
  exp, log, sqrt :: a -> a
  sin, cos, tan, asin, acos, atan :: a -> a
  sinh, cosh, tanh, asinh, acosh, atanh :: a -> a
  (**), logBase :: a -> a -> a
  x ** y = exp (log x * y)
  logBase x y = log y / log x
  sqrt x = x ** 0.5
  tan x = sin x / cos x
  tanh x = sinh x / cosh x

class (Real a, Fractional a) => RealFrac a where
  properFraction :: Integral b => a -> (b, a)
  truncate, round, ceiling, floor :: Integral b => a -> b
  truncate x = fst (properFraction x)

class (RealFrac a, Floating a) => RealFloat a where
  isNaN :: a -> Bool
  isInfinite :: a -> Bool
  isNegativeZero :: a -> Bool
  floatDigits :: a -> Int
  decodeFloat :: a -> (Integer, Int)
  atan2 :: a -> a -> a

instance Num Int where
  (+) = primIntAdd
  (-) = primIntSub
  (*) = primIntMul
  negate = primIntNegate
  abs = primIntAbs
  signum = primIntSignum
  fromInteger = primIntegerToInt

instance Real Int where
  toRational x = primIntegerToRatio (primIntToInteger x)

instance Integral Int where
  quot = primIntQuot
  rem = primIntRem
  div = primIntDiv
  mod = primIntMod
  quotRem = primIntQuotRem
  divMod = primIntDivMod
  toInteger = primIntToInteger

instance Num Integer where
  (+) = primIntegerAdd
  (-) = primIntegerSub
  (*) = primIntegerMul
  negate = primIntegerNegate
  abs = primIntegerAbs
  signum = primIntegerSignum
  fromInteger x = x

instance Real Integer where
  toRational = primIntegerToRatio

instance Integral Integer where
  quot = primIntegerQuot
  rem = primIntegerRem
  div = primIntegerDiv
  mod = primIntegerMod
  quotRem = primIntegerQuotRem
  divMod = primIntegerDivMod
  toInteger x = x

instance Num Double where
  (+) = primDoubleAdd
  (-) = primDoubleSub
  (*) = primDoubleMul
  negate = primDoubleNegate
  abs = primDoubleAbs
  signum = primDoubleSignum
  fromInteger = primIntegerToDouble

instance Real Double where
  toRational = primDoubleToRational

instance Fractional Double where
  (/) = primDoubleDiv
  recip x = primDoubleDiv 1.0 x
  fromRational = primRationalToDouble

instance Floating Double where
  pi = 3.141592653589793
  exp = primExp
  log = primLog
  sqrt = primSqrt
  sin = primSin
  cos = primCos
  tan = primTan
  asin = primAsin
  acos = primAcos
  atan = primAtan
  sinh = primSinh
  cosh = primCosh
  tanh = primTanh
  asinh = primAsinh
  acosh = primAcosh
  atanh = primAtanh
  (**) = primPowDouble
  logBase b x = primLog x / primLog b

instance RealFrac Double where
  properFraction x = let n = primDoubleTruncate x
                     in (fromInteger n, x - primIntegerToDouble n)
  truncate x = fromInteger (primDoubleTruncate x)
  round x = fromInteger (primDoubleRound x)
  ceiling x = fromInteger (primDoubleCeiling x)
  floor x = fromInteger (primDoubleFloor x)

instance RealFloat Double where
  isNaN = primIsNaN
  isInfinite = primIsInfinite
  isNegativeZero x = x == 0 && primShowDouble x == "-0.0"
  floatDigits _ = 53
  decodeFloat = primDecodeDouble
  atan2 = primAtan2

-- Float: single precision.  A Float is a Double rounded to the nearest
-- single-precision value after every operation (for + - * / and sqrt this
-- gives exactly the IEEE single-precision result).
newtype Float = MkFloat Double deriving (Eq, Ord)

float1 :: (Double -> Double) -> Float -> Float
float1 f (MkFloat x) = MkFloat (primFloat32 (f x))

float2 :: (Double -> Double -> Double) -> Float -> Float -> Float
float2 f (MkFloat x) (MkFloat y) = MkFloat (primFloat32 (f x y))

floatToDouble :: Float -> Double
floatToDouble (MkFloat x) = x

doubleToFloat :: Double -> Float
doubleToFloat x = MkFloat (primFloat32 x)

instance Num Float where
  (+) = float2 primDoubleAdd
  (-) = float2 primDoubleSub
  (*) = float2 primDoubleMul
  negate = float1 primDoubleNegate
  abs = float1 primDoubleAbs
  signum = float1 primDoubleSignum
  fromInteger n = doubleToFloat (primIntegerToDouble n)

instance Real Float where
  toRational (MkFloat x) = primDoubleToRational x

instance Fractional Float where
  (/) = float2 primDoubleDiv
  fromRational r = doubleToFloat (primRationalToDouble r)

instance Floating Float where
  pi = doubleToFloat 3.141592653589793
  exp = float1 primExp
  log = float1 primLog
  sqrt = float1 primSqrt
  sin = float1 primSin
  cos = float1 primCos
  tan = float1 primTan
  asin = float1 primAsin
  acos = float1 primAcos
  atan = float1 primAtan
  sinh = float1 primSinh
  cosh = float1 primCosh
  tanh = float1 primTanh
  asinh = float1 primAsinh
  acosh = float1 primAcosh
  atanh = float1 primAtanh
  (**) = float2 primPowDouble
  logBase b x = log x / log b

instance RealFrac Float where
  properFraction (MkFloat x) = let (n, f) = properFraction x in (n, doubleToFloat f)
  truncate (MkFloat x) = truncate x
  round (MkFloat x) = round x
  ceiling (MkFloat x) = ceiling x
  floor (MkFloat x) = floor x

instance RealFloat Float where
  isNaN (MkFloat x) = isNaN x
  isInfinite (MkFloat x) = isInfinite x
  isNegativeZero (MkFloat x) = isNegativeZero x
  floatDigits _ = 24
  decodeFloat (MkFloat x)
    | x == 0 = (0, 0)
    | otherwise = let (m, e) = decodeFloat x in (m `quot` 536870912, e + 29)
  atan2 = float2 primAtan2

instance Enum Float where
  succ x = x + 1
  pred x = x - 1
  toEnum n = fromIntegral n
  fromEnum x = truncate x
  enumFrom = numericEnumFrom
  enumFromThen = numericEnumFromThen
  enumFromTo = numericEnumFromTo
  enumFromThenTo = numericEnumFromThenTo

subtract :: Num a => a -> a -> a
subtract x y = y - x

even, odd :: Integral a => a -> Bool
even n = n `rem` 2 == 0
odd n = not (even n)

gcd :: Integral a => a -> a -> a
gcd x y = fromInteger (primIntegerGcd (toInteger x) (toInteger y))

lcm :: Integral a => a -> a -> a
lcm x y = fromInteger (primIntegerLcm (toInteger x) (toInteger y))

(^) :: (Num a, Integral b) => a -> b -> a
x0 ^ y0
  | y0 < 0 = error "Negative exponent"
  | y0 == 0 = 1
  | otherwise = f x0 y0
  where
    -- GHC's algorithm (it needs only (*), not fromInteger)
    f x y | even y = f (x * x) (y `quot` 2)
          | y == 1 = x
          | otherwise = g (x * x) (y `quot` 2) x
    g x y z | even y = g (x * x) (y `quot` 2) z
            | y == 1 = x * z
            | otherwise = g (x * x) (y `quot` 2) (x * z)

(^^) :: (Fractional a, Integral b) => a -> b -> a
x ^^ n = if n >= 0 then x ^ n else recip (x ^ negate n)

fromIntegral :: (Integral a, Num b) => a -> b
fromIntegral x = fromInteger (toInteger x)

realToFrac :: (Real a, Fractional b) => a -> b
realToFrac x = fromRational (toRational x)

-- Ratio

(%) :: Integral a => a -> a -> Ratio a
x % y = primMakeRatio (toInteger x) (toInteger y)

numerator :: Ratio a -> a
numerator r = primUnsafeCoerce (primNumerator r)

denominator :: Ratio a -> a
denominator r = primUnsafeCoerce (primDenominator r)

instance Eq (Ratio a) where
  (==) = primRatioEq

instance Ord (Ratio a) where
  compare = primCompareRatio
  (<) = primRatioLt
  (<=) = primRatioLe

instance Integral a => Num (Ratio a) where
  (+) = primRatioAdd
  (-) = primRatioSub
  (*) = primRatioMul
  negate = primRatioNegate
  abs = primRatioAbs
  signum = primRatioSignum
  fromInteger = primIntegerToRatio

instance Integral a => Real (Ratio a) where
  toRational = primRatioToRational

instance Integral a => Fractional (Ratio a) where
  (/) = primRatioDiv
  fromRational = primRationalToRatio

instance Integral a => RealFrac (Ratio a) where
  properFraction x = let n = primRatioTruncate x
                     in (fromInteger n, x - primIntegerToRatio n)
  truncate x = fromInteger (primRatioTruncate x)
  round x = fromInteger (primRatioRound x)
  ceiling x = fromInteger (primRatioCeiling x)
  floor x = fromInteger (primRatioFloor x)

instance (Show a, Integral a) => Show (Ratio a) where
  showsPrec d r = showParen (d > 7) (showsPrec 8 (numerator r) . showString " % " . showsPrec 8 (denominator r))

instance Integral a => Enum (Ratio a) where
  succ x = x + 1
  pred x = x - 1
  toEnum n = fromInteger (primIntToInteger n)
  fromEnum x = primIntegerToInt (primRatioTruncate x)
  enumFrom = numericEnumFrom
  enumFromThen = numericEnumFromThen
  enumFromTo = numericEnumFromTo
  enumFromThenTo = numericEnumFromThenTo

------------------------------------------------------------------------
-- Semigroup and Monoid

class Semigroup a where
  (<>) :: a -> a -> a
  sconcat :: NonEmpty a -> a
  stimes :: Integral b => b -> a -> a
  sconcat (a :| as) = go a as
    where go b (c : cs) = b <> go c cs
          go b [] = b
  stimes n x = if n <= 1 then x else x <> stimes (n - 1) x

class Semigroup a => Monoid a where
  mempty :: a
  mappend :: a -> a -> a
  mconcat :: [a] -> a
  mappend = (<>)
  mconcat xs = foldr mappend mempty xs

-- non-empty lists (Data.List.NonEmpty)
infixr 5 :|
data NonEmpty a = a :| [a] deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Semigroup (NonEmpty a) where
  (a :| as) <> (b :| bs) = a :| (as ++ b : bs)

instance Semigroup [a] where
  (<>) = (++)

instance Monoid [a] where
  mempty = []
  mconcat = concat

instance Semigroup Ordering where
  LT <> _ = LT
  EQ <> y = y
  GT <> _ = GT

instance Monoid Ordering where
  mempty = EQ

instance Semigroup () where
  _ <> _ = ()

instance Monoid () where
  mempty = ()

instance Semigroup a => Semigroup (Maybe a) where
  Nothing <> b = b
  a <> Nothing = a
  Just a <> Just b = Just (a <> b)

instance Semigroup a => Monoid (Maybe a) where
  mempty = Nothing

instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
  (a, b) <> (c, d) = (a <> c, b <> d)

instance (Monoid a, Monoid b) => Monoid (a, b) where
  mempty = (mempty, mempty)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (a, b, c) where
  (a, b, c) <> (d, e, f) = (a <> d, b <> e, c <> f)

instance (Monoid a, Monoid b, Monoid c) => Monoid (a, b, c) where
  mempty = (mempty, mempty, mempty)

instance Semigroup b => Semigroup (a -> b) where
  f <> g = \x -> f x <> g x

instance Monoid b => Monoid (a -> b) where
  mempty = \_ -> mempty

newtype Sum a = Sum { getSum :: a } deriving (Eq, Ord, Show)
newtype Product a = Product { getProduct :: a } deriving (Eq, Ord, Show)
newtype Min a = Min { getMin :: a } deriving (Eq, Ord, Show)
newtype Max a = Max { getMax :: a } deriving (Eq, Ord, Show)
newtype First a = First { getFirst :: Maybe a } deriving (Eq, Ord, Show)
newtype Last a = Last { getLast :: Maybe a } deriving (Eq, Ord, Show)
newtype Any = Any { getAny :: Bool } deriving (Eq, Ord, Show)
newtype All = All { getAll :: Bool } deriving (Eq, Ord, Show)
newtype Endo a = Endo { appEndo :: a -> a }

instance Num a => Semigroup (Sum a) where
  Sum a <> Sum b = Sum (a + b)
instance Num a => Monoid (Sum a) where
  mempty = Sum 0
instance Num a => Semigroup (Product a) where
  Product a <> Product b = Product (a * b)
instance Num a => Monoid (Product a) where
  mempty = Product 1
instance Ord a => Semigroup (Min a) where
  Min a <> Min b = Min (min a b)
instance (Ord a, Bounded a) => Monoid (Min a) where
  mempty = Min maxBound
instance Ord a => Semigroup (Max a) where
  Max a <> Max b = Max (max a b)
instance (Ord a, Bounded a) => Monoid (Max a) where
  mempty = Max minBound
instance Semigroup (First a) where
  First Nothing <> b = b
  a <> _ = a
instance Monoid (First a) where
  mempty = First Nothing
instance Semigroup (Last a) where
  a <> Last Nothing = a
  _ <> b = b
instance Monoid (Last a) where
  mempty = Last Nothing
instance Semigroup Any where
  Any a <> Any b = Any (a || b)
instance Monoid Any where
  mempty = Any False
instance Semigroup All where
  All a <> All b = All (a && b)
instance Monoid All where
  mempty = All True
instance Semigroup (Endo a) where
  Endo f <> Endo g = Endo (f . g)
instance Monoid (Endo a) where
  mempty = Endo id

------------------------------------------------------------------------
-- Functor, Applicative, Monad

class Functor f where
  fmap :: (a -> b) -> f a -> f b
  (<$) :: a -> f b -> f a
  x <$ m = fmap (\_ -> x) m

class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
  liftA2 :: (a -> b -> c) -> f a -> f b -> f c
  (*>) :: f a -> f b -> f b
  (<*) :: f a -> f b -> f a
  mf <*> mx = liftA2 id mf mx
  liftA2 f x y = fmap f x <*> y
  a *> b = liftA2 (\_ y -> y) a b
  a <* b = liftA2 (\x _ -> x) a b

class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
  m >> k = m >>= \_ -> k
  return = pure

class Monad m => MonadFail m where
  fail :: String -> m a

class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a
  some :: f a -> f [a]
  many :: f a -> f [a]
  some v = liftA2 (:) v (many v)
  many v = some v <|> pure []

class (Alternative m, Monad m) => MonadPlus m where
  mzero :: m a
  mplus :: m a -> m a -> m a
  mzero = empty
  mplus = (<|>)

(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap

(<&>) :: Functor f => f a -> (a -> b) -> f b
x <&> f = fmap f x

($>) :: Functor f => f a -> b -> f b
x $> y = fmap (\_ -> y) x

(<**>) :: Applicative f => f a -> f (a -> b) -> f b
(<**>) = liftA2 (\a f -> f a)

void :: Functor f => f a -> f ()
void x = () <$ x

liftA :: Applicative f => (a -> b) -> f a -> f b
liftA f a = pure f <*> a

liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 f a b c = liftA2 f a b <*> c

(=<<) :: Monad m => (a -> m b) -> m a -> m b
f =<< x = x >>= f

(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
f >=> g = \x -> f x >>= g

(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
g <=< f = \x -> f x >>= g

join :: Monad m => m (m a) -> m a
join x = x >>= id

liftM :: Monad m => (a -> r) -> m a -> m r
liftM f m = m >>= \x -> return (f x)

liftM2 :: Monad m => (a -> b -> r) -> m a -> m b -> m r
liftM2 f m1 m2 = m1 >>= \x -> m2 >>= \y -> return (f x y)

ap :: Monad m => m (a -> b) -> m a -> m b
ap mf mx = mf >>= \f -> mx >>= \x -> return (f x)

when :: Applicative f => Bool -> f () -> f ()
when p s = if p then s else pure ()

unless :: Applicative f => Bool -> f () -> f ()
unless p s = if p then pure () else s

guard :: Alternative f => Bool -> f ()
guard True = pure ()
guard False = empty

forever :: Applicative f => f a -> f b
forever a = let a' = a *> a' in a'

replicateM :: Applicative m => Int -> m a -> m [a]
replicateM n m
  | n <= 0 = pure []
  | otherwise = liftA2 (:) m (replicateM (n - 1) m)

replicateM_ :: Applicative m => Int -> m a -> m ()
replicateM_ n m
  | n <= 0 = pure ()
  | otherwise = m *> replicateM_ (n - 1) m

foldM :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
foldM f z0 xs = go z0 (toList xs)
  where go z [] = return z
        go z (y:ys) = f z y >>= \z' -> go z' ys

foldM_ :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m ()
foldM_ f z xs = foldM f z xs >> return ()

zipWithM :: Applicative m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM f xs ys = sequenceA (zipWith f xs ys)

zipWithM_ :: Applicative m => (a -> b -> m c) -> [a] -> [b] -> m ()
zipWithM_ f xs ys = sequenceA_ (zipWith f xs ys)

filterM :: Applicative m => (a -> m Bool) -> [a] -> m [a]
filterM p = foldr (\x -> liftA2 (\flg -> if flg then (x:) else id) (p x)) (pure [])

mapAndUnzipM :: Applicative m => (a -> m (b, c)) -> [a] -> m ([b], [c])
mapAndUnzipM f xs = unzip <$> traverse f xs

msum :: (Foldable t, MonadPlus m) => t (m a) -> m a
msum = foldr mplus mzero

asum :: (Foldable t, Alternative f) => t (f a) -> f a
asum = foldr (<|>) empty

-- lists

instance Functor [] where
  fmap = map

instance Applicative [] where
  pure x = [x]
  fs <*> xs = [f x | f <- fs, x <- xs]
  liftA2 f xs ys = [f x y | x <- xs, y <- ys]
  xs *> ys = [y | _ <- xs, y <- ys]

instance Monad [] where
  xs >>= f = concatMap f xs

instance MonadFail [] where
  fail _ = []

instance Alternative [] where
  empty = []
  (<|>) = (++)

instance MonadPlus []

-- Maybe

instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap f (Just x) = Just (f x)

instance Applicative Maybe where
  pure = Just
  Just f <*> m = fmap f m
  Nothing <*> _ = Nothing
  liftA2 f (Just x) (Just y) = Just (f x y)
  liftA2 _ _ _ = Nothing
  Just _ *> m = m
  Nothing *> _ = Nothing

instance Monad Maybe where
  (Just x) >>= k = k x
  Nothing >>= _ = Nothing

instance MonadFail Maybe where
  fail _ = Nothing

instance Alternative Maybe where
  empty = Nothing
  Nothing <|> r = r
  l <|> _ = l

instance MonadPlus Maybe

-- Either

instance Functor (Either e) where
  fmap _ (Left x) = Left x
  fmap f (Right y) = Right (f y)

instance Applicative (Either e) where
  pure = Right
  Left e <*> _ = Left e
  Right f <*> r = fmap f r

instance Monad (Either e) where
  Left l >>= _ = Left l
  Right r >>= k = k r

-- functions

instance Functor ((->) r) where
  fmap = (.)

instance Applicative ((->) r) where
  pure = const
  f <*> g = \x -> f x (g x)
  liftA2 q f g = \x -> q (f x) (g x)

instance Monad ((->) r) where
  f >>= k = \r -> k (f r) r

-- pairs

instance Functor ((,) a) where
  fmap f (x, y) = (x, f y)

instance Monoid a => Applicative ((,) a) where
  pure x = (mempty, x)
  (u, f) <*> (v, x) = (u <> v, f x)

instance Monoid a => Monad ((,) a) where
  (u, a) >>= k = case k a of (v, b) -> (u <> v, b)

-- IO

instance Functor IO where
  fmap f m = primBindIO m (\x -> primReturnIO (f x))

instance Applicative IO where
  pure = primReturnIO
  mf <*> mx = primBindIO mf (\f -> primBindIO mx (\x -> primReturnIO (f x)))
  liftA2 f ma mb = primBindIO ma (\a -> primBindIO mb (\b -> primReturnIO (f a b)))
  (*>) = primThenIO
  ma <* mb = primBindIO ma (\a -> primBindIO mb (\_ -> primReturnIO a))

instance Monad IO where
  (>>=) = primBindIO
  (>>) = primThenIO
  return = primReturnIO

instance MonadFail IO where
  fail s = ioError (userError s)

instance Semigroup a => Semigroup (IO a) where
  a <> b = liftA2 (<>) a b

instance Monoid a => Monoid (IO a) where
  mempty = pure mempty

------------------------------------------------------------------------
-- Foldable and Traversable

class Foldable t where
  foldMap :: Monoid m => (a -> m) -> t a -> m
  foldr :: (a -> b -> b) -> b -> t a -> b
  foldl :: (b -> a -> b) -> b -> t a -> b
  foldr' :: (a -> b -> b) -> b -> t a -> b
  foldl' :: (b -> a -> b) -> b -> t a -> b
  foldr1 :: (a -> a -> a) -> t a -> a
  foldl1 :: (a -> a -> a) -> t a -> a
  toList :: t a -> [a]
  null :: t a -> Bool
  length :: t a -> Int
  elem :: Eq a => a -> t a -> Bool
  maximum :: Ord a => t a -> a
  minimum :: Ord a => t a -> a
  sum :: Num a => t a -> a
  product :: Num a => t a -> a
  foldMap f t = foldr (\x acc -> f x <> acc) mempty t
  foldr f z t = appEndo (foldMap (\x -> Endo (f x)) t) z
  foldl f z t = listFoldl f z (toList t)
  foldr' f z t = listFoldr f z (toList t)
  foldl' f z t = listFoldl' f z (toList t)
  foldr1 f t = listFoldr1 f (toList t)
  foldl1 f t = listFoldl1 f (toList t)
  toList t = foldr (:) [] t
  null t = listNull (toList t)
  length t = listLength (toList t)
  elem x t = listElem x (toList t)
  maximum t = listMaximum (toList t)
  minimum t = listMinimum (toList t)
  sum t = listFoldl' (+) 0 (toList t)
  product t = listFoldl' (*) 1 (toList t)

class (Functor t, Foldable t) => Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  sequenceA :: Applicative f => t (f a) -> f (t a)
  mapM :: Monad m => (a -> m b) -> t a -> m (t b)
  sequence :: Monad m => t (m a) -> m (t a)
  sequenceA t = traverse id t
  traverse f t = sequenceA (fmap f t)
  mapM f t = traverse f t
  sequence t = sequenceA t

instance Foldable [] where
  foldMap f xs = mconcat (map f xs)
  foldr = listFoldr
  foldl = listFoldl
  foldr' = listFoldr
  foldl' = listFoldl'
  foldr1 = listFoldr1
  foldl1 = listFoldl1
  toList xs = xs
  null = listNull
  length = listLength
  elem = listElem
  maximum = listMaximum
  minimum = listMinimum
  sum = listFoldl' (+) 0
  product = listFoldl' (*) 1

instance Traversable [] where
  traverse f = listFoldr (\x ys -> liftA2 (:) (f x) ys) (pure [])
  mapM f = listFoldr (\x ys -> f x >>= \y -> ys >>= \zs -> return (y : zs)) (return [])

instance Foldable Maybe where
  foldr _ z Nothing = z
  foldr f z (Just x) = f x z
  toList Nothing = []
  toList (Just x) = [x]
  null Nothing = True
  null (Just _) = False
  length Nothing = 0
  length (Just _) = 1

instance Traversable Maybe where
  traverse _ Nothing = pure Nothing
  traverse f (Just x) = Just <$> f x

instance Foldable (Either a) where
  foldr _ z (Left _) = z
  foldr f z (Right y) = f y z
  toList (Left _) = []
  toList (Right y) = [y]

instance Traversable (Either a) where
  traverse _ (Left x) = pure (Left x)
  traverse f (Right y) = Right <$> f y

instance Foldable ((,) a) where
  foldr f z (_, y) = f y z
  toList (_, y) = [y]

instance Traversable ((,) a) where
  traverse f (x, y) = (\z -> (x, z)) <$> f y

listFoldr :: (a -> b -> b) -> b -> [a] -> b
listFoldr f z = go
  where go [] = z
        go (y:ys) = f y (go ys)

listFoldl :: (b -> a -> b) -> b -> [a] -> b
listFoldl f z [] = z
listFoldl f z (x:xs) = listFoldl f (f z x) xs

listFoldl' :: (b -> a -> b) -> b -> [a] -> b
listFoldl' f z [] = z
listFoldl' f z (x:xs) = let z' = f z x in z' `seq` listFoldl' f z' xs

listFoldr1 :: (a -> a -> a) -> [a] -> a
listFoldr1 f [x] = x
listFoldr1 f (x:xs) = f x (listFoldr1 f xs)
listFoldr1 _ [] = errorEmptyList "foldr1"

listFoldl1 :: (a -> a -> a) -> [a] -> a
listFoldl1 f (x:xs) = listFoldl f x xs
listFoldl1 _ [] = errorEmptyList "foldl1"

listNull :: [a] -> Bool
listNull [] = True
listNull _ = False

listLength :: [a] -> Int
listLength xs = go 0 xs
  where go !n [] = n
        go !n (_:ys) = go (n + 1) ys

listElem :: Eq a => a -> [a] -> Bool
listElem _ [] = False
listElem x (y:ys) = x == y || listElem x ys

listMaximum :: Ord a => [a] -> a
listMaximum [] = errorEmptyList "maximum"
listMaximum (x:xs) = go x xs
  where go !m [] = m
        go !m (y:ys) = go (if y > m then y else m) ys

listMinimum :: Ord a => [a] -> a
listMinimum [] = errorEmptyList "minimum"
listMinimum (x:xs) = go x xs
  where go !m [] = m
        go !m (y:ys) = go (if y < m then y else m) ys

errorEmptyList :: String -> a
errorEmptyList fun = errorWithoutStackTrace ("Prelude." ++ fun ++ ": empty list")

concat :: Foldable t => t [a] -> [a]
concat xss = listFoldr (++) [] (toList xss)

concatMap :: Foldable t => (a -> [b]) -> t a -> [b]
concatMap f xs = listFoldr (\x acc -> f x ++ acc) [] (toList xs)

and :: Foldable t => t Bool -> Bool
and xs = listAnd (toList xs)
  where listAnd [] = True
        listAnd (y:ys) = y && listAnd ys

or :: Foldable t => t Bool -> Bool
or xs = listOr (toList xs)
  where listOr [] = False
        listOr (y:ys) = y || listOr ys

any :: Foldable t => (a -> Bool) -> t a -> Bool
any p xs = go (toList xs)
  where go [] = False
        go (y:ys) = p y || go ys

all :: Foldable t => (a -> Bool) -> t a -> Bool
all p xs = go (toList xs)
  where go [] = True
        go (y:ys) = p y && go ys

notElem :: (Foldable t, Eq a) => a -> t a -> Bool
notElem x t = not (elem x t)

find :: Foldable t => (a -> Bool) -> t a -> Maybe a
find p t = go (toList t)
  where go [] = Nothing
        go (y:ys) = if p y then Just y else go ys

maximumBy :: Foldable t => (a -> a -> Ordering) -> t a -> a
maximumBy cmp t = case toList t of
  [] -> errorEmptyList "maximumBy"
  (x:xs) -> listFoldl (\m y -> case cmp y m of { LT -> m; EQ -> y; GT -> y }) x xs

minimumBy :: Foldable t => (a -> a -> Ordering) -> t a -> a
minimumBy cmp t = case toList t of
  [] -> errorEmptyList "minimumBy"
  (x:xs) -> listFoldl (\m y -> case cmp y m of { LT -> y; _ -> m }) x xs

mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
mapM_ f t = go (toList t)
  where go [] = return ()
        go (x:xs) = f x >> go xs

forM_ :: (Foldable t, Monad m) => t a -> (a -> m b) -> m ()
forM_ t f = mapM_ f t

forM :: (Traversable t, Monad m) => t a -> (a -> m b) -> m (t b)
forM t f = mapM f t

sequence_ :: (Foldable t, Monad m) => t (m a) -> m ()
sequence_ t = go (toList t)
  where go [] = return ()
        go (m:ms) = m >> go ms

traverse_ :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f ()
traverse_ f t = listFoldr (\x k -> f x *> k) (pure ()) (toList t)

for_ :: (Foldable t, Applicative f) => t a -> (a -> f b) -> f ()
for_ t f = traverse_ f t

sequenceA_ :: (Foldable t, Applicative f) => t (f a) -> f ()
sequenceA_ t = listFoldr (*>) (pure ()) (toList t)

for :: (Traversable t, Applicative f) => t a -> (a -> f b) -> f (t b)
for t f = traverse f t

fold :: (Foldable t, Monoid m) => t m -> m
fold t = foldMap id t

------------------------------------------------------------------------
-- Lists

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

(++) :: [a] -> [a] -> [a]
(++) [] ys = ys
(++) (x:xs) ys = x : (xs ++ ys)

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs) = if p x then x : filter p xs else filter p xs

head :: [a] -> a
head (x:_) = x
head [] = errorEmptyList "head"

tail :: [a] -> [a]
tail (_:xs) = xs
tail [] = errorEmptyList "tail"

last :: [a] -> a
last [x] = x
last (_:xs) = last xs
last [] = errorEmptyList "last"

init :: [a] -> [a]
init [x] = []
init (x:xs) = x : init xs
init [] = errorEmptyList "init"

uncons :: [a] -> Maybe (a, [a])
uncons [] = Nothing
uncons (x:xs) = Just (x, xs)

unsnoc :: [a] -> Maybe ([a], a)
unsnoc [] = Nothing
unsnoc xs = Just (init xs, last xs)

singleton :: a -> [a]
singleton x = [x]

(!!) :: [a] -> Int -> a
xs !! n | n < 0 = errorWithoutStackTrace "Prelude.!!: negative index"
[] !! _ = errorWithoutStackTrace "Prelude.!!: index too large"
(x:_) !! 0 = x
(_:xs) !! n = xs !! (n - 1)

(!?) :: [a] -> Int -> Maybe a
xs !? n
  | n < 0 = Nothing
  | otherwise = case drop n xs of
      (y:_) -> Just y
      [] -> Nothing

reverse :: [a] -> [a]
reverse l = rev l []
  where rev [] a = a
        rev (x:xs) a = rev xs (x : a)

take :: Int -> [a] -> [a]
take n _ | n <= 0 = []
take _ [] = []
take n (x:xs) = x : take (n - 1) xs

drop :: Int -> [a] -> [a]
drop n xs | n <= 0 = xs
drop _ [] = []
drop n (_:xs) = drop (n - 1) xs

splitAt :: Int -> [a] -> ([a], [a])
splitAt n xs = (take n xs, drop n xs)

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x:xs) = if p x then x : takeWhile p xs else []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile p xs@(x:xs') = if p x then dropWhile p xs' else xs

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p = foldr (\x xs -> if p x && null xs then [] else x : xs) []

span :: (a -> Bool) -> [a] -> ([a], [a])
span _ [] = ([], [])
span p xs@(x:xs')
  | p x = let (ys, zs) = span p xs' in (x : ys, zs)
  | otherwise = ([], xs)

break :: (a -> Bool) -> [a] -> ([a], [a])
break p = span (not . p)

stripPrefix :: Eq a => [a] -> [a] -> Maybe [a]
stripPrefix [] ys = Just ys
stripPrefix (x:xs) (y:ys) | x == y = stripPrefix xs ys
stripPrefix _ _ = Nothing

lookup :: Eq a => a -> [(a, b)] -> Maybe b
lookup _ [] = Nothing
lookup key ((x, y) : rest) = if key == x then Just y else lookup key rest

zip :: [a] -> [b] -> [(a, b)]
zip (a:as) (b:bs) = (a, b) : zip as bs
zip _ _ = []

zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3 (a:as) (b:bs) (c:cs) = (a, b, c) : zip3 as bs cs
zip3 _ _ _ = []

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f (a:as) (b:bs) = f a b : zipWith f as bs
zipWith _ _ _ = []

zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWith3 z (a:as) (b:bs) (c:cs) = z a b c : zipWith3 z as bs cs
zipWith3 _ _ _ _ = []

unzip :: [(a, b)] -> ([a], [b])
unzip = foldr (\(a, b) ~(as, bs) -> (a : as, b : bs)) ([], [])

unzip3 :: [(a, b, c)] -> ([a], [b], [c])
unzip3 = foldr (\(a, b, c) ~(as, bs, cs) -> (a : as, b : bs, c : cs)) ([], [], [])

iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)

iterate' :: (a -> a) -> a -> [a]
iterate' f x = x `seq` (x : iterate' f (f x))

repeat :: a -> [a]
repeat x = let xs = x : xs in xs

replicate :: Int -> a -> [a]
replicate n x = if n <= 0 then [] else x : replicate (n - 1) x

cycle :: [a] -> [a]
cycle [] = errorEmptyList "cycle"
cycle xs = let xs' = xs ++ xs' in xs'

scanl :: (b -> a -> b) -> b -> [a] -> [b]
scanl f q ls = q : (case ls of
                      [] -> []
                      x:xs -> scanl f (f q x) xs)

scanl' :: (b -> a -> b) -> b -> [a] -> [b]
scanl' f q ls = q `seq` (q : (case ls of
                                [] -> []
                                x:xs -> scanl' f (f q x) xs))

scanl1 :: (a -> a -> a) -> [a] -> [a]
scanl1 f (x:xs) = scanl f x xs
scanl1 _ [] = []

scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr _ q0 [] = [q0]
scanr f q0 (x:xs) = case scanr f q0 xs of
  qs@(q:_) -> f x q : qs

scanr1 :: (a -> a -> a) -> [a] -> [a]
scanr1 _ [] = []
scanr1 _ [x] = [x]
scanr1 f (x:xs) = case scanr1 f xs of
  qs@(q:_) -> f x q : qs

lines :: String -> [String]
lines = primLines

foreign import prim "hs-lines" primLines :: String -> [String]

unlines :: [String] -> String
unlines [] = []
unlines (l:ls) = l ++ '\n' : unlines ls

words :: String -> [String]
words = primWords

foreign import prim "hs-words" primWords :: String -> [String]

unwords :: [String] -> String
unwords [] = ""
unwords [w] = w
unwords (w:ws) = w ++ ' ' : unwords ws

-- Data.List

foldl1' :: (a -> a -> a) -> [a] -> a
foldl1' f (x:xs) = listFoldl' f x xs
foldl1' _ [] = errorEmptyList "foldl1'"

elemIndex :: Eq a => a -> [a] -> Maybe Int
elemIndex x = findIndex (== x)

elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices x = findIndices (== x)

findIndex :: (a -> Bool) -> [a] -> Maybe Int
findIndex p xs = case findIndices p xs of
  [] -> Nothing
  (i:_) -> Just i

findIndices :: (a -> Bool) -> [a] -> [Int]
findIndices p xs = [i | (x, i) <- zip xs [0 ..], p x]

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf x y = reverse x `isPrefixOf` reverse y

isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)

isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf a@(x:a') (y:b)
  | x == y = isSubsequenceOf a' b
  | otherwise = isSubsequenceOf a b

nub :: Eq a => [a] -> [a]
nub = nubBy (==)

nubBy :: (a -> a -> Bool) -> [a] -> [a]
nubBy eq l = go l []
  where go [] _ = []
        go (y:ys) xs
          | any (eq y) xs = go ys xs
          | otherwise = y : go ys (y : xs)

delete :: Eq a => a -> [a] -> [a]
delete = deleteBy (==)

deleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy _ _ [] = []
deleteBy eq x (y:ys) = if eq x y then ys else y : deleteBy eq x ys

(\\) :: Eq a => [a] -> [a] -> [a]
(\\) = foldl (flip delete)

union :: Eq a => [a] -> [a] -> [a]
union = unionBy (==)

unionBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
unionBy eq xs ys = xs ++ foldl (flip (deleteBy eq)) (nubBy eq ys) xs

intersect :: Eq a => [a] -> [a] -> [a]
intersect = intersectBy (==)

intersectBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
intersectBy _ [] _ = []
intersectBy _ _ [] = []
intersectBy eq xs ys = [x | x <- xs, any (eq x) ys]

insert :: Ord a => a -> [a] -> [a]
insert = insertBy compare

insertBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
insertBy _ x [] = [x]
insertBy cmp x ys@(y:ys') = case cmp x y of
  GT -> y : insertBy cmp x ys'
  _ -> x : ys

intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse sep (x:xs) = x : go xs
  where go [] = []
        go (y:ys) = sep : y : go ys

intercalate :: [a] -> [[a]] -> [a]
intercalate xs xss = concat (intersperse xs xss)

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([] : xss) = transpose xss
transpose ((x:xs) : xss) = (x : [h | (h:_) <- xss]) : transpose (xs : [t | (_:t) <- xss])

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p xs = foldr select ([], []) xs
  where select x ~(ts, fs) = if p x then (x : ts, fs) else (ts, x : fs)

group :: Eq a => [a] -> [[a]]
group = groupBy (==)

groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _ [] = []
groupBy eq (x:xs) = let (ys, zs) = span (eq x) xs in (x : ys) : groupBy eq zs

inits :: [a] -> [[a]]
inits xs = [] : case xs of
  [] -> []
  (y:ys) -> map (y:) (inits ys)

tails :: [a] -> [[a]]
tails xs = xs : case xs of
  [] -> []
  (_:ys) -> tails ys

subsequences :: [a] -> [[a]]
subsequences xs = [] : nonEmptySubsequences xs

nonEmptySubsequences :: [a] -> [[a]]
nonEmptySubsequences [] = []
nonEmptySubsequences (x:xs) = [x] : foldr f [] (nonEmptySubsequences xs)
  where f ys r = ys : (x : ys) : r

permutations :: [a] -> [[a]]
permutations xs0 = xs0 : perms xs0 []
  where
    perms [] _ = []
    perms (t:ts) is' = foldr interleave (perms ts (t : is')) (permutations is')
      where interleave xs r = let (_, zs) = interleave' id xs r in zs
            interleave' _ [] r = (ts, r)
            interleave' f (y:ys) r = let (us, zs) = interleave' (f . (y:)) ys r
                                     in (y : us, f (t : y : us) : zs)

sort :: Ord a => [a] -> [a]
sort = sortBy compare

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f = map snd . sortBy (comparing fst) . map (\x -> let y = f x in y `seq` (y, x))

-- merge sort (as in GHC's Data.List)
sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy cmp = mergeAll . sequences
  where
    sequences (a:b:xs) = case cmp a b of
      GT -> descending b [a] xs
      _ -> ascending b (\ys -> a : ys) xs
    sequences xs = [xs]
    descending a as (b:bs) | cmp a b == GT = descending b (a : as) bs
    descending a as bs = (a : as) : sequences bs
    ascending a as (b:bs) | cmp a b /= GT = ascending b (\ys -> as (a : ys)) bs
    ascending a as bs = let x = as [a] in x `seq` (x : sequences bs)
    mergeAll [x] = x
    mergeAll xs = mergeAll (mergePairs xs)
    mergePairs (a:b:xs) = let x = merge a b in x `seq` (x : mergePairs xs)
    mergePairs xs = xs
    merge as@(a:as') bs@(b:bs') = case cmp a b of
      GT -> b : merge as bs'
      _ -> a : merge as' bs
    merge [] bs = bs
    merge as [] = as

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f b = case f b of
  Just (a, b') -> a : unfoldr f b'
  Nothing -> []

mapAccumL :: (s -> a -> (s, b)) -> s -> [a] -> (s, [b])
mapAccumL _ s [] = (s, [])
mapAccumL f s (x:xs) = let (s', y) = f s x
                           (s'', ys) = mapAccumL f s' xs
                       in (s'', y : ys)

mapAccumR :: (s -> a -> (s, b)) -> s -> [a] -> (s, [b])
mapAccumR _ s [] = (s, [])
mapAccumR f s (x:xs) = let (s', ys) = mapAccumR f s xs
                           (s'', y) = f s' x
                       in (s'', y : ys)

genericLength :: Num i => [a] -> i
genericLength xs = fromIntegral (length xs)

genericTake :: Integral i => i -> [a] -> [a]
genericTake n xs = take (fromIntegral n) xs

genericDrop :: Integral i => i -> [a] -> [a]
genericDrop n xs = drop (fromIntegral n) xs

genericReplicate :: Integral i => i -> a -> [a]
genericReplicate n x = replicate (fromIntegral n) x

genericSplitAt :: Integral i => i -> [a] -> ([a], [a])
genericSplitAt n xs = splitAt (fromIntegral n) xs

genericIndex :: Integral i => [a] -> i -> a
genericIndex xs n = xs !! fromIntegral n

zip4 :: [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
zip4 (a:as) (b:bs) (c:cs) (d:ds) = (a, b, c, d) : zip4 as bs cs ds
zip4 _ _ _ _ = []

zipWith4 :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
zipWith4 z (a:as) (b:bs) (c:cs) (d:ds) = z a b c d : zipWith4 z as bs cs ds
zipWith4 _ _ _ _ _ = []

lookupDefault :: Eq a => b -> a -> [(a, b)] -> b
lookupDefault d k xs = maybe d id (lookup k xs)

-- Data.Maybe

fromMaybe :: a -> Maybe a -> a
fromMaybe d Nothing = d
fromMaybe _ (Just x) = x

fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust Nothing = errorWithoutStackTrace "Maybe.fromJust: Nothing"

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

catMaybes :: [Maybe a] -> [a]
catMaybes ms = [x | Just x <- ms]

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = []
mapMaybe f (x:xs) = case f x of
  Nothing -> mapMaybe f xs
  Just y -> y : mapMaybe f xs

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

-- Data.Either

lefts :: [Either a b] -> [a]
lefts xs = [x | Left x <- xs]

rights :: [Either a b] -> [b]
rights xs = [x | Right x <- xs]

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers = foldr (either left right) ([], [])
  where left a ~(l, r) = (a : l, r)
        right a ~(l, r) = (l, a : r)

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

fromLeft :: a -> Either a b -> a
fromLeft _ (Left a) = a
fromLeft a _ = a

fromRight :: b -> Either a b -> b
fromRight _ (Right b) = b
fromRight b _ = b

-- Data.Char

isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

isOctDigit :: Char -> Bool
isOctDigit c = c >= '0' && c <= '7'

isHexDigit :: Char -> Bool
isHexDigit c = isDigit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

isAlphaNum :: Char -> Bool
isAlphaNum c = isAlpha c || isNumber c

isNumber :: Char -> Bool
isNumber c = isDigit c || (ord c > 127 && primIsNumeric c)

foreign import prim "char-numeric?" primIsNumeric :: Char -> Bool

isAscii :: Char -> Bool
isAscii c = ord c < 128

isLatin1 :: Char -> Bool
isLatin1 c = ord c < 256

isAsciiUpper :: Char -> Bool
isAsciiUpper c = c >= 'A' && c <= 'Z'

isAsciiLower :: Char -> Bool
isAsciiLower c = c >= 'a' && c <= 'z'

isControl :: Char -> Bool
isControl c = ord c < 32 || (ord c >= 127 && ord c < 160)

isPrint :: Char -> Bool
isPrint c = not (isControl c)

isSeparator :: Char -> Bool
isSeparator c = c == ' ' || c == '\xa0'

digitToInt :: Char -> Int
digitToInt c
  | isDigit c = ord c - ord '0'
  | c >= 'a' && c <= 'f' = ord c - ord 'a' + 10
  | c >= 'A' && c <= 'F' = ord c - ord 'A' + 10
  | otherwise = error ("Char.digitToInt: not a digit " ++ show c)

intToDigit :: Int -> Char
intToDigit i
  | i >= 0 && i <= 9 = chr (ord '0' + i)
  | i >= 10 && i <= 15 = chr (ord 'a' + i - 10)
  | otherwise = error ("Char.intToDigit: not a digit " ++ show i)

------------------------------------------------------------------------
-- Show

class Show a where
  showsPrec :: Int -> a -> ShowS
  show :: a -> String
  showList :: [a] -> ShowS
  showsPrec _ x s = show x ++ s
  show x = showsPrec 0 x ""
  showList ls s = showListWith shows ls s

showListWith :: (a -> ShowS) -> [a] -> ShowS
showListWith _ [] s = "[]" ++ s
showListWith showx (x:xs) s = '[' : showx x (showl xs)
  where showl [] = ']' : s
        showl (y:ys) = ',' : showx y (showl ys)

shows :: Show a => a -> ShowS
shows = showsPrec 0

showChar :: Char -> ShowS
showChar = (:)

showString :: String -> ShowS
showString = (++)

showParen :: Bool -> ShowS -> ShowS
showParen b p = if b then showChar '(' . p . showChar ')' else p

showSpace :: ShowS
showSpace = \xs -> ' ' : xs

instance Show Int where
  showsPrec d n r = if n < 0 && d > 6 then '(' : primShowInt n ++ (')' : r) else primShowInt n ++ r
  show = primShowInt

instance Show Integer where
  showsPrec d n r = if n < 0 && d > 6 then '(' : primShowInteger n ++ (')' : r) else primShowInteger n ++ r
  show = primShowInteger

instance Show Double where
  showsPrec d x r = if x < 0 && d > 6 then '(' : primShowDouble x ++ (')' : r) else primShowDouble x ++ r
  show = primShowDouble

instance Show Char where
  showsPrec _ c s = primShowChar c ++ s
  showList cs s = primShowString cs ++ s

instance Show a => Show [a] where
  showsPrec _ = showList

instance Show () where
  showsPrec _ () s = '(' : ')' : s

instance (Show a, Show b) => Show (a, b) where
  showsPrec _ (a, b) s = '(' : shows a (',' : shows b (')' : s))

instance (Show a, Show b, Show c) => Show (a, b, c) where
  showsPrec _ (a, b, c) s = '(' : shows a (',' : shows b (',' : shows c (')' : s)))

instance (Show a, Show b, Show c, Show d) => Show (a, b, c, d) where
  showsPrec _ (a, b, c, d) s = '(' : shows a (',' : shows b (',' : shows c (',' : shows d (')' : s))))

instance (Show a, Show b, Show c, Show d, Show e) => Show (a, b, c, d, e) where
  showsPrec _ (a, b, c, d, e) s =
    '(' : shows a (',' : shows b (',' : shows c (',' : shows d (',' : shows e (')' : s)))))

instance (Show a, Show b, Show c, Show d, Show e, Show f) => Show (a, b, c, d, e, f) where
  showsPrec _ (a, b, c, d, e, f) s =
    '(' : shows a (',' : shows b (',' : shows c (',' : shows d (',' : shows e (',' : shows f (')' : s))))))

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g) => Show (a, b, c, d, e, f, g) where
  showsPrec _ (a, b, c, d, e, f, g) s =
    '(' : shows a (',' : shows b (',' : shows c (',' : shows d (',' : shows e (',' : shows f (',' : shows g (')' : s)))))))

instance Show SomeException where
  show = primShowException

instance Show IOException where
  show (IOError e) = primShowException e

instance Show ErrorCall where
  show (ErrorCall m) = m

instance Show ArithException where
  show Overflow = "arithmetic overflow"
  show Underflow = "arithmetic underflow"
  show LossOfPrecision = "loss of precision"
  show DivideByZero = "divide by zero"
  show Denormal = "denormal"
  show RatioZeroDenominator = "Ratio has zero denominator"

-- Numeric

showHex :: Integral a => a -> ShowS
showHex n = showString (primShowHex (toInteger n))

showOct :: Integral a => a -> ShowS
showOct n = showString (primShowOct (toInteger n))

showFFloat :: RealFloat a => Maybe Int -> a -> ShowS
showFFloat d x = showString (primShowFFloat (maybe (-1) id d) (floatDigits x == 24) (realToDouble x))

showEFloat :: RealFloat a => Maybe Int -> a -> ShowS
showEFloat d x = showString (primShowEFloat (maybe (-1) id d) (floatDigits x == 24) (realToDouble x))

showGFloat :: RealFloat a => Maybe Int -> a -> ShowS
showGFloat d x = let y = abs (realToDouble x)
                 in if y /= 0 && (y < 0.1 || y >= 10000000) then showEFloat d x else showFFloat d x

realToDouble :: Real a => a -> Double
realToDouble x = primRationalToDouble (toRational x)

showInt :: Integral a => a -> ShowS
showInt n = showString (primShowInteger (toInteger n))

readHex :: (Eq a, Num a) => ReadS a
readHex s = case span isHexDigit s of
  ([], _) -> []
  (ds, rest) -> [(foldl (\acc c -> acc * 16 + fromIntegral (digitToInt c)) 0 ds, rest)]

readOct :: (Eq a, Num a) => ReadS a
readOct s = case span isOctDigit s of
  ([], _) -> []
  (ds, rest) -> [(foldl (\acc c -> acc * 8 + fromIntegral (digitToInt c)) 0 ds, rest)]

readDec :: (Eq a, Num a) => ReadS a
readDec s = case span isDigit s of
  ([], _) -> []
  (ds, rest) -> [(foldl (\acc c -> acc * 10 + fromIntegral (digitToInt c)) 0 ds, rest)]

------------------------------------------------------------------------
-- Read

class Read a where
  readsPrec :: Int -> ReadS a
  readList :: ReadS [a]
  readList = readListDefault

readListDefault :: Read a => ReadS [a]
readListDefault = readParen False (\r -> [pr | ("[", s) <- lex r, pr <- readl s])
  where readl s = [([], t) | ("]", t) <- lex s] ++
                  [(x : xs, u) | (x, t) <- reads s, (xs, u) <- readl' t]
        readl' s = [([], t) | ("]", t) <- lex s] ++
                   [(x : xs, v) | (",", t) <- lex s, (x, u) <- reads t, (xs, v) <- readl' u]

reads :: Read a => ReadS a
reads = readsPrec 0

read :: Read a => String -> a
read s = case [x | (x, t) <- reads s, all isSpace t] of
  [x] -> x
  [] -> errorWithoutStackTrace "Prelude.read: no parse"
  _ -> errorWithoutStackTrace "Prelude.read: ambiguous parse"

readMaybe :: Read a => String -> Maybe a
readMaybe s = case [x | (x, t) <- reads s, ("", "") <- lex t] of
  [x] -> Just x
  _ -> Nothing

readEither :: Read a => String -> Either String a
readEither s = case [x | (x, t) <- reads s, ("", "") <- lex t] of
  [x] -> Right x
  [] -> Left "Prelude.read: no parse"
  _ -> Left "Prelude.read: ambiguous parse"

readLn :: Read a => IO a
readLn = getLine >>= \l -> readIO l

readIO :: Read a => String -> IO a
readIO s = case [x | (x, t) <- reads s, ("", "") <- lex t] of
  [x] -> return x
  [] -> ioError (userError "Prelude.readIO: no parse")
  _ -> ioError (userError "Prelude.readIO: ambiguous parse")

readParen :: Bool -> ReadS a -> ReadS a
readParen b g = if b then mandatory else optional
  where optional r = g r ++ mandatory r
        mandatory r = [(x, u) | ("(", s) <- lex r, (x, t) <- optional s, (")", u) <- lex t]

-- a simplified version of the Haskell 2010 lexer
lex :: ReadS String
lex "" = [("", "")]
lex (c:s)
  | isSpace c = lex (dropWhile isSpace s)
lex ('\'':s) = [('\'' : ch ++ "'", t) | (ch, '\'':t) <- lexLitChar s, ch /= "'"]
lex ('"':s) = [('"' : str, t) | (str, t) <- lexString s]
  where
    lexString ('"':s') = [("\"", s')]
    lexString s' = [(ch ++ str, u) | (ch, t) <- lexStrItem s', (str, u) <- lexString t]
    lexStrItem ('\\':'&':s') = [("\\&", s')]
    lexStrItem s' = lexLitChar s'
lex (c:s)
  | isSingle c = [([c], s)]
  | isSym c = [(c : sym, t) | (sym, t) <- [span isSym s]]
  | isAlpha c || c == '_' = [(c : nam, t) | (nam, t) <- [span isIdChar s]]
  | isDigit c = [(c : ds ++ fe, t) | (ds, s') <- [span isDigit s], (fe, t) <- lexFracExp s']
  | otherwise = []
  where
    isSingle x = x `elem` ",;()[]{}`"
    isSym x = x `elem` "!@#$%&*+./<=>?\\^|:-~"
    isIdChar x = isAlphaNum x || x == '_' || x == '\''
    lexFracExp ('.':x:cs) | isDigit x = [('.' : ds ++ e, u) | (ds, t) <- lexDigits (x:cs), (e, u) <- lexExp t]
    lexFracExp s' = lexExp s'
    lexExp (e:s') | e `elem` "eE" = [(e : c' : ds, u) | (c':t) <- [s'], c' `elem` "+-", (ds, u) <- lexDigits t] ++
                                    [(e : ds, t) | (ds, t) <- lexDigits s']
    lexExp s' = [("", s')]

lexDigits :: ReadS String
lexDigits s = case span isDigit s of
  ([], _) -> []
  (ds, rest) -> [(ds, rest)]

lexLitChar :: ReadS String
lexLitChar ('\\':c:s) = [('\\' : c : ds, t) | (ds, t) <- [span isDigit s]]
lexLitChar (c:s) = [([c], s)]
lexLitChar "" = []

instance Read Int where
  readsPrec _ s = case primReadsInteger s of
    [] -> readParenNum s
    rs -> [(primIntegerToInt n, t) | (n, t) <- rs]

instance Read Integer where
  readsPrec _ s = case primReadsInteger s of
    [] -> readParenNum s
    rs -> rs

instance Read Float where
  readsPrec d s = [(doubleToFloat x, r) | (x, r) <- readsPrec d s]

instance Show Float where
  showsPrec d (MkFloat x) = showParen (d > 6 && x < 0) (showString (primShowFloat x))

instance Read Double where
  readsPrec _ s = case primReadsDouble s of
    [] -> readParenNum s
    rs -> rs

-- a number in parentheses, e.g. "(-5)"
readParenNum :: Read a => ReadS a
readParenNum r = [(x, u) | ("(", s') <- lex r, (x, t) <- readsPrec 0 s', (")", u) <- lex t]

instance Read Char where
  readsPrec _ = primReadsChar
  readList s = primReadsString s ++ readListDefault s

instance Read a => Read [a] where
  readsPrec _ = readList

instance Read () where
  readsPrec _ r = [((), t) | ("(", s) <- lex r, (")", t) <- lex s]

instance (Read a, Read b) => Read (a, b) where
  readsPrec _ r = [((a, b), w) | ("(", s) <- lex r, (a, t) <- reads s, (",", u) <- lex t,
                                 (b, v) <- reads u, (")", w) <- lex v]

instance (Read a, Read b, Read c) => Read (a, b, c) where
  readsPrec _ r = [((a, b, c), y) | ("(", s) <- lex r, (a, t) <- reads s, (",", u) <- lex t,
                                    (b, v) <- reads u, (",", w) <- lex v, (c, x) <- reads w,
                                    (")", y) <- lex x]

-- used by derived Read instances
primReadCons :: Int -> [(String, ReadS a)] -> ReadS a
primReadCons d alts = readParen False go
  where go s = [r | (tok, rest) <- lex s, (name, rd) <- alts, tok == name, r <- rd rest]

-- a record field "name = value" followed by the token next ("," or "}")
primReadField :: Read a => String -> String -> ReadS a
primReadField name next s =
  [(v, s4) | s1 <- fieldName s, ("=", s2) <- lex s1,
             (v, s3) <- readsPrec 0 s2, (tok, s4) <- lex s3, tok == next]
  where
    -- an operator field is shown in parentheses: (+++) = ...
    fieldName s
      | isAlpha (head name) || head name == '_' = primReadToken name s
      | otherwise = [s3 | s1 <- primReadToken "(" s, s2 <- primReadToken name s1, s3 <- primReadToken ")" s2]

primReadToken :: String -> String -> [String]
primReadToken t s = [rest | (tok, rest) <- lex s, tok == t]

------------------------------------------------------------------------
-- IO

putStrLn :: String -> IO ()
putStrLn s = putStr s >> putChar '\n'

print :: Show a => a -> IO ()
print x = putStrLn (show x)

interact :: (String -> String) -> IO ()
interact f = getContents >>= \s -> putStr (f s)

hPutStrLn :: Handle -> String -> IO ()
hPutStrLn h s = hPutStr h s >> hPutStr h "\n"

hPrint :: Show a => Handle -> a -> IO ()
hPrint h x = hPutStrLn h (show x)

data BufferMode = NoBuffering | LineBuffering | BlockBuffering (Maybe Int)
  deriving (Eq, Show)

foreign import prim "hs-set-tty-mode" primSetTtyMode :: Handle -> Int -> Bool -> IO ()

-- (only stdin's buffering matters: NoBuffering reads keys as they are
-- typed; output is flushed before reading input anyway)
hSetBuffering :: Handle -> BufferMode -> IO ()
hSetBuffering h m = primSetTtyMode h 1 (m /= NoBuffering)

hGetBuffering :: Handle -> IO BufferMode
hGetBuffering _ = return LineBuffering

hSetEcho :: Handle -> Bool -> IO ()
hSetEcho h on = primSetTtyMode h 0 on

data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode
  deriving (Eq, Ord, Show, Enum)

openFile :: FilePath -> IOMode -> IO Handle
openFile path mode = primOpenFile path (fromEnum mode)

withFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
withFile path mode act = openFile path mode >>= \h -> act h >>= \r -> hClose h >> return r

foreign import prim "hs-does-file-exist" primFileExists :: FilePath -> IO Bool

-- creates and opens a new file in dir: "temp" -> dir/temp123, "x.txt" -> dir/x123.txt
openTempFile :: FilePath -> String -> IO (FilePath, Handle)
openTempFile dir template = getCPUTime >>= \t -> go (fromInteger (t `div` 1000000 `mod` 100000))
  where
    (base, ext) = case break (== '.') (reverse template) of
                    (rext, '.' : rbase) -> (reverse rbase, '.' : reverse rext)
                    _ -> (template, "")
    go n = let path = dir ++ "/" ++ base ++ show (n :: Int) ++ ext
           in primFileExists path >>= \e ->
                if e then go (n + 1) else openFile path WriteMode >>= \h -> return (path, h)

modifyIORef :: IORef a -> (a -> a) -> IO ()
modifyIORef r f = readIORef r >>= \x -> writeIORef r (f x)

modifyIORef' :: IORef a -> (a -> a) -> IO ()
modifyIORef' r f = readIORef r >>= \x -> let x' = f x in x' `seq` writeIORef r x'

atomicModifyIORef :: IORef a -> (a -> (a, b)) -> IO b
atomicModifyIORef r f = readIORef r >>= \x -> case f x of (a, b) -> writeIORef r a >> return b

atomicModifyIORef' :: IORef a -> (a -> (a, b)) -> IO b
atomicModifyIORef' r f = readIORef r >>= \x -> case f x of (a, b) -> a `seq` writeIORef r a >> return b

atomicWriteIORef :: IORef a -> a -> IO ()
atomicWriteIORef = writeIORef

-- Exceptions.  A SomeException is the raised Scheme object; its payload
-- tells which Haskell type it holds (see runtime.ss).  User-defined
-- exception types work like GHC's: the compiler gives each
-- `instance Exception T` a primExceptionTag that identifies T, so the
-- default toException/fromException need no Typeable.

class Show e => Exception e where
  toException :: e -> SomeException
  fromException :: SomeException -> Maybe e
  displayException :: e -> String
  primExceptionTag :: e -> String
  toException e = primMakeTyped (primExceptionTag e) (show e) e
  -- (primExceptionTag ignores its argument, so the knot is never forced)
  fromException se = r
    where r = primExceptionMatch (primExceptionTag (unJust r)) se
          unJust (Just x) = x
  displayException = show

instance Exception SomeException where
  toException e = e
  fromException e = Just e
  primExceptionTag _ = "SomeException"

-- raised by error, undefined and failed pattern matches
data ErrorCall = ErrorCall String

instance Eq ErrorCall where
  ErrorCall a == ErrorCall b = a == b

instance Exception ErrorCall where
  toException (ErrorCall m) = primMakeErrorCall m
  fromException e = if primExceptionKind e == 0 then Just (ErrorCall (primErrorCallMessage e)) else Nothing
  primExceptionTag _ = "ErrorCall"

newtype IOException = IOError SomeException

instance Eq IOException where
  a == b = show a == show b

instance Exception IOException where
  toException (IOError e) = e
  fromException e = if primExceptionKind e == 1 then Just (IOError e) else Nothing
  primExceptionTag _ = "IOException"

data ArithException = Overflow | Underflow | LossOfPrecision | DivideByZero | Denormal
                    | RatioZeroDenominator
  deriving (Eq, Ord)

instance Exception ArithException where
  toException e = primMakeArith (show e)
  fromException e
    | primExceptionKind e == 2 = lookup (primShowException e) [(show a, a) | a <- arithExceptions]
    | otherwise = Nothing
  primExceptionTag _ = "ArithException"

arithExceptions :: [ArithException]
arithExceptions = [Overflow, Underflow, LossOfPrecision, DivideByZero, Denormal, RatioZeroDenominator]

-- handlers only see exceptions of their argument type; others propagate
catch :: Exception e => IO a -> (e -> IO a) -> IO a
catch a h = primCatch a (\se -> case fromException se of
                                  Just e -> h e
                                  Nothing -> primRaise se)

ioError :: IOError -> IO a
ioError = throwIO

userError :: String -> IOError
userError s = IOError (primMakeIOError ("user error (" ++ s ++ ")"))

throwIO :: Exception e => e -> IO a
throwIO e = primRaiseIO (toException e)

throw :: Exception e => e -> a
throw e = primRaise (toException e)

try :: Exception e => IO a -> IO (Either e a)
try a = catch (a >>= \v -> return (Right v)) (\e -> return (Left e))

handle :: Exception e => (e -> IO a) -> IO a -> IO a
handle h a = catch a h

catchJust :: Exception e => (e -> Maybe b) -> IO a -> (b -> IO a) -> IO a
catchJust p a h = catch a (\e -> maybe (throwIO e) h (p e))

handleJust :: Exception e => (e -> Maybe b) -> (b -> IO a) -> IO a -> IO a
handleJust p h a = catchJust p a h

tryJust :: Exception e => (e -> Maybe b) -> IO a -> IO (Either b a)
tryJust p a = catch (fmap Right a) (\e -> maybe (throwIO e) (return . Left) (p e))

bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracket acquire release use = acquire >>= \a ->
  primCatch (use a) (\e -> release a >> primRaiseIO e) >>= \r -> release a >> return r

finally :: IO a -> IO b -> IO a
finally a sequel = primCatch a (\e -> sequel >> primRaiseIO e) >>= \r -> sequel >> return r

bracket_ :: IO a -> IO b -> IO c -> IO c
bracket_ before after thing = bracket before (const after) (const thing)

-- like bracket, but only releases when the action fails
bracketOnError :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracketOnError acquire release use = acquire >>= \a -> primCatch (use a) (\e -> release a >> primRaiseIO e)

onException :: IO a -> IO b -> IO a
onException a what = primCatch a (\e -> what >> primRaiseIO e)

-- System.Exit

data ExitCode = ExitSuccess | ExitFailure Int deriving (Eq, Ord, Show, Read)

exitWith :: ExitCode -> IO a
exitWith ExitSuccess = primExitWith 0
exitWith (ExitFailure n) = primExitWith n

exitSuccess :: IO a
exitSuccess = primExitWith 0

exitFailure :: IO a
exitFailure = primExitWith 1

die :: String -> IO a
die msg = hPutStrLn stderr msg >> exitFailure

-- Debug.Trace

trace :: String -> a -> a
trace = primTrace

traceShow :: Show a => a -> b -> b
traceShow x y = primTrace (show x) y

traceShowId :: Show a => a -> a
traceShowId x = primTrace (show x) x

traceId :: String -> String
traceId s = primTrace s s

traceM :: Applicative f => String -> f ()
traceM s = primTrace s (pure ())

traceShowM :: (Show a, Applicative f) => a -> f ()
traceShowM x = primTrace (show x) (pure ())

------------------------------------------------------------------------
-- Data.Bits

class Eq a => Bits a where
  (.&.), (.|.), xor :: a -> a -> a
  complement :: a -> a
  shift, shiftL, shiftR :: a -> Int -> a
  testBit :: a -> Int -> Bool
  popCount :: a -> Int
  bit :: Int -> a
  setBit, clearBit, complementBit :: a -> Int -> a
  zeroBits :: a
  shift x n = if n >= 0 then shiftL x n else shiftR x (negate n)

instance Bits Int where
  a .&. b = primIntegerToInt (primAnd (primIntToInteger a) (primIntToInteger b))
  a .|. b = primIntegerToInt (primOr (primIntToInteger a) (primIntToInteger b))
  xor a b = primIntegerToInt (primXor (primIntToInteger a) (primIntToInteger b))
  complement a = primIntegerToInt (primComplement (primIntToInteger a))
  shiftL a n = primIntShiftL a n
  shiftR a n = primIntegerToInt (primShiftR (primIntToInteger a) n)
  testBit a n = n < 64 && primTestBit (primIntToInteger a) n
  popCount a = primIntPopCount a
  bit n = primIntShiftL 1 n
  setBit a n = a .|. bit n
  clearBit a n = a .&. complement (bit n)
  complementBit a n = a `xor` bit n
  zeroBits = 0

instance Bits Integer where
  (.&.) = primAnd
  (.|.) = primOr
  xor = primXor
  complement = primComplement
  shiftL = primShiftL
  shiftR = primShiftR
  testBit = primTestBit
  popCount = primPopCount
  bit n = primShiftL 1 n
  setBit a n = a .|. bit n
  clearBit a n = a .&. complement (bit n)
  complementBit a n = a `xor` bit n
  zeroBits = 0

------------------------------------------------------------------------
-- Sized integers.  Int itself is 64-bit and wraps around (see the
-- primitives).  Int8 ... Word64 and Word are newtypes over Integer whose
-- operations wrap to the type's size; the class SizedInt gives the generic
-- definitions below their size.  (Int64 is a synonym for Int.)

class SizedInt a where
  sizedBits :: a -> Int
  sizedSigned :: a -> Bool
  sizedTo :: a -> Integer
  sizedRaw :: Integer -> a       -- no wrapping

sizedFrom :: SizedInt a => Integer -> a
sizedFrom x = r where r = sizedRaw (primWrapBits (sizedBits r) (sizedSigned r) x)

sized2 :: SizedInt a => (Integer -> Integer -> Integer) -> a -> a -> a
sized2 f a b = sizedFrom (f (sizedTo a) (sizedTo b))

sizedMin, sizedMax :: SizedInt a => a
sizedMin = r where r = sizedRaw (if sizedSigned r then negate (2 ^ (sizedBits r - 1)) else 0)
sizedMax = r where r = sizedRaw (if sizedSigned r then 2 ^ (sizedBits r - 1) - 1 else 2 ^ sizedBits r - 1)

sizedShowsPrec :: SizedInt a => Int -> a -> ShowS
sizedShowsPrec d x = showsPrec d (sizedTo x)

sizedReadsPrec :: SizedInt a => Int -> ReadS a
sizedReadsPrec d s = [(sizedFrom x, r) | (x, r) <- readsPrec d s]

sizedToEnum :: SizedInt a => String -> Int -> a
sizedToEnum name n
  | toInteger n >= sizedTo (sizedMin `asTypeOf` r) && toInteger n <= sizedTo (sizedMax `asTypeOf` r) = r
  | otherwise = error ("Enum.toEnum{" ++ name ++ "}: tag (" ++ show n ++ ") is outside of bounds")
  where r = sizedRaw (toInteger n)

sizedEnumFromTo :: SizedInt a => a -> a -> [a]
sizedEnumFromTo a b = map sizedRaw (enumFromTo (sizedTo a) (sizedTo b))

sizedEnumFromThenTo :: SizedInt a => a -> a -> a -> [a]
sizedEnumFromThenTo a b c = map sizedRaw (enumFromThenTo (sizedTo a) (sizedTo b) (sizedTo c))

sizedQuotRem, sizedDivMod :: SizedInt a => a -> a -> (a, a)
sizedQuotRem a b = let (q, r) = quotRem (sizedTo a) (sizedTo b) in (sizedFrom q, sizedFrom r)
sizedDivMod a b = let (q, r) = divMod (sizedTo a) (sizedTo b) in (sizedFrom q, sizedFrom r)

sizedShiftR :: SizedInt a => a -> Int -> a
sizedShiftR a n = sizedFrom (primShiftR (sizedTo a) n)

sizedTestBit :: SizedInt a => a -> Int -> Bool
sizedTestBit a n = n < sizedBits a && primTestBit (sizedTo a) n

sizedPopCount :: SizedInt a => a -> Int
sizedPopCount a = primPopCount (primWrapBits (sizedBits a) False (sizedTo a))


newtype Int8 = MkInt8 Integer deriving (Eq, Ord)
instance SizedInt Int8 where
  sizedBits _ = 8
  sizedSigned _ = True
  sizedTo (MkInt8 x) = x
  sizedRaw = MkInt8
instance Show Int8 where showsPrec = sizedShowsPrec
instance Read Int8 where readsPrec = sizedReadsPrec
instance Bounded Int8 where { minBound = sizedMin; maxBound = sizedMax }
instance Num Int8 where
  { (+) = sized2 (+); (-) = sized2 (-); (*) = sized2 (*); negate = sizedFrom . negate . sizedTo
  ; abs = sizedFrom . abs . sizedTo; signum = sizedFrom . signum . sizedTo; fromInteger = sizedFrom }
instance Real Int8 where toRational = toRational . sizedTo
instance Enum Int8 where
  { toEnum = sizedToEnum "Int8"; fromEnum = primIntegerToInt . sizedTo
  ; succ x = if x == maxBound then error "Enum.succ{Int8}: tried to take `succ' of maxBound" else x + 1
  ; pred x = if x == minBound then error "Enum.pred{Int8}: tried to take `pred' of minBound" else x - 1
  ; enumFrom x = sizedEnumFromTo x maxBound; enumFromTo = sizedEnumFromTo
  ; enumFromThen x y = sizedEnumFromThenTo x y (if y >= x then maxBound else minBound)
  ; enumFromThenTo = sizedEnumFromThenTo }
instance Integral Int8 where
  { quot = sized2 quot; rem = sized2 rem; div = sized2 div; mod = sized2 mod
  ; quotRem = sizedQuotRem; divMod = sizedDivMod; toInteger = sizedTo }
instance Ix Int8 where
  { range = uncurry sizedEnumFromTo; index (m, _) i = primIntegerToInt (sizedTo i - sizedTo m)
  ; inRange (m, n) i = m <= i && i <= n }
instance Bits Int8 where
  { (.&.) = sized2 primAnd; (.|.) = sized2 primOr; xor = sized2 primXor
  ; complement = sizedFrom . primComplement . sizedTo; shiftL a n = sizedFrom (primShiftL (sizedTo a) n)
  ; shiftR = sizedShiftR; testBit = sizedTestBit; popCount = sizedPopCount; bit n = sizedFrom (primShiftL 1 n)
  ; setBit a n = a .|. bit n; clearBit a n = a .&. complement (bit n); complementBit a n = a `xor` bit n
  ; zeroBits = 0 }

newtype Int16 = MkInt16 Integer deriving (Eq, Ord)
instance SizedInt Int16 where
  sizedBits _ = 16
  sizedSigned _ = True
  sizedTo (MkInt16 x) = x
  sizedRaw = MkInt16
instance Show Int16 where showsPrec = sizedShowsPrec
instance Read Int16 where readsPrec = sizedReadsPrec
instance Bounded Int16 where { minBound = sizedMin; maxBound = sizedMax }
instance Num Int16 where
  { (+) = sized2 (+); (-) = sized2 (-); (*) = sized2 (*); negate = sizedFrom . negate . sizedTo
  ; abs = sizedFrom . abs . sizedTo; signum = sizedFrom . signum . sizedTo; fromInteger = sizedFrom }
instance Real Int16 where toRational = toRational . sizedTo
instance Enum Int16 where
  { toEnum = sizedToEnum "Int16"; fromEnum = primIntegerToInt . sizedTo
  ; succ x = if x == maxBound then error "Enum.succ{Int16}: tried to take `succ' of maxBound" else x + 1
  ; pred x = if x == minBound then error "Enum.pred{Int16}: tried to take `pred' of minBound" else x - 1
  ; enumFrom x = sizedEnumFromTo x maxBound; enumFromTo = sizedEnumFromTo
  ; enumFromThen x y = sizedEnumFromThenTo x y (if y >= x then maxBound else minBound)
  ; enumFromThenTo = sizedEnumFromThenTo }
instance Integral Int16 where
  { quot = sized2 quot; rem = sized2 rem; div = sized2 div; mod = sized2 mod
  ; quotRem = sizedQuotRem; divMod = sizedDivMod; toInteger = sizedTo }
instance Ix Int16 where
  { range = uncurry sizedEnumFromTo; index (m, _) i = primIntegerToInt (sizedTo i - sizedTo m)
  ; inRange (m, n) i = m <= i && i <= n }
instance Bits Int16 where
  { (.&.) = sized2 primAnd; (.|.) = sized2 primOr; xor = sized2 primXor
  ; complement = sizedFrom . primComplement . sizedTo; shiftL a n = sizedFrom (primShiftL (sizedTo a) n)
  ; shiftR = sizedShiftR; testBit = sizedTestBit; popCount = sizedPopCount; bit n = sizedFrom (primShiftL 1 n)
  ; setBit a n = a .|. bit n; clearBit a n = a .&. complement (bit n); complementBit a n = a `xor` bit n
  ; zeroBits = 0 }

newtype Int32 = MkInt32 Integer deriving (Eq, Ord)
instance SizedInt Int32 where
  sizedBits _ = 32
  sizedSigned _ = True
  sizedTo (MkInt32 x) = x
  sizedRaw = MkInt32
instance Show Int32 where showsPrec = sizedShowsPrec
instance Read Int32 where readsPrec = sizedReadsPrec
instance Bounded Int32 where { minBound = sizedMin; maxBound = sizedMax }
instance Num Int32 where
  { (+) = sized2 (+); (-) = sized2 (-); (*) = sized2 (*); negate = sizedFrom . negate . sizedTo
  ; abs = sizedFrom . abs . sizedTo; signum = sizedFrom . signum . sizedTo; fromInteger = sizedFrom }
instance Real Int32 where toRational = toRational . sizedTo
instance Enum Int32 where
  { toEnum = sizedToEnum "Int32"; fromEnum = primIntegerToInt . sizedTo
  ; succ x = if x == maxBound then error "Enum.succ{Int32}: tried to take `succ' of maxBound" else x + 1
  ; pred x = if x == minBound then error "Enum.pred{Int32}: tried to take `pred' of minBound" else x - 1
  ; enumFrom x = sizedEnumFromTo x maxBound; enumFromTo = sizedEnumFromTo
  ; enumFromThen x y = sizedEnumFromThenTo x y (if y >= x then maxBound else minBound)
  ; enumFromThenTo = sizedEnumFromThenTo }
instance Integral Int32 where
  { quot = sized2 quot; rem = sized2 rem; div = sized2 div; mod = sized2 mod
  ; quotRem = sizedQuotRem; divMod = sizedDivMod; toInteger = sizedTo }
instance Ix Int32 where
  { range = uncurry sizedEnumFromTo; index (m, _) i = primIntegerToInt (sizedTo i - sizedTo m)
  ; inRange (m, n) i = m <= i && i <= n }
instance Bits Int32 where
  { (.&.) = sized2 primAnd; (.|.) = sized2 primOr; xor = sized2 primXor
  ; complement = sizedFrom . primComplement . sizedTo; shiftL a n = sizedFrom (primShiftL (sizedTo a) n)
  ; shiftR = sizedShiftR; testBit = sizedTestBit; popCount = sizedPopCount; bit n = sizedFrom (primShiftL 1 n)
  ; setBit a n = a .|. bit n; clearBit a n = a .&. complement (bit n); complementBit a n = a `xor` bit n
  ; zeroBits = 0 }

newtype Word = MkWord Integer deriving (Eq, Ord)
instance SizedInt Word where
  sizedBits _ = 64
  sizedSigned _ = False
  sizedTo (MkWord x) = x
  sizedRaw = MkWord
instance Show Word where showsPrec = sizedShowsPrec
instance Read Word where readsPrec = sizedReadsPrec
instance Bounded Word where { minBound = sizedMin; maxBound = sizedMax }
instance Num Word where
  { (+) = sized2 (+); (-) = sized2 (-); (*) = sized2 (*); negate = sizedFrom . negate . sizedTo
  ; abs = sizedFrom . abs . sizedTo; signum = sizedFrom . signum . sizedTo; fromInteger = sizedFrom }
instance Real Word where toRational = toRational . sizedTo
instance Enum Word where
  { toEnum = sizedToEnum "Word"; fromEnum = primIntegerToInt . sizedTo
  ; succ x = if x == maxBound then error "Enum.succ{Word}: tried to take `succ' of maxBound" else x + 1
  ; pred x = if x == minBound then error "Enum.pred{Word}: tried to take `pred' of minBound" else x - 1
  ; enumFrom x = sizedEnumFromTo x maxBound; enumFromTo = sizedEnumFromTo
  ; enumFromThen x y = sizedEnumFromThenTo x y (if y >= x then maxBound else minBound)
  ; enumFromThenTo = sizedEnumFromThenTo }
instance Integral Word where
  { quot = sized2 quot; rem = sized2 rem; div = sized2 div; mod = sized2 mod
  ; quotRem = sizedQuotRem; divMod = sizedDivMod; toInteger = sizedTo }
instance Ix Word where
  { range = uncurry sizedEnumFromTo; index (m, _) i = primIntegerToInt (sizedTo i - sizedTo m)
  ; inRange (m, n) i = m <= i && i <= n }
instance Bits Word where
  { (.&.) = sized2 primAnd; (.|.) = sized2 primOr; xor = sized2 primXor
  ; complement = sizedFrom . primComplement . sizedTo; shiftL a n = sizedFrom (primShiftL (sizedTo a) n)
  ; shiftR = sizedShiftR; testBit = sizedTestBit; popCount = sizedPopCount; bit n = sizedFrom (primShiftL 1 n)
  ; setBit a n = a .|. bit n; clearBit a n = a .&. complement (bit n); complementBit a n = a `xor` bit n
  ; zeroBits = 0 }

newtype Word8 = MkWord8 Integer deriving (Eq, Ord)
instance SizedInt Word8 where
  sizedBits _ = 8
  sizedSigned _ = False
  sizedTo (MkWord8 x) = x
  sizedRaw = MkWord8
instance Show Word8 where showsPrec = sizedShowsPrec
instance Read Word8 where readsPrec = sizedReadsPrec
instance Bounded Word8 where { minBound = sizedMin; maxBound = sizedMax }
instance Num Word8 where
  { (+) = sized2 (+); (-) = sized2 (-); (*) = sized2 (*); negate = sizedFrom . negate . sizedTo
  ; abs = sizedFrom . abs . sizedTo; signum = sizedFrom . signum . sizedTo; fromInteger = sizedFrom }
instance Real Word8 where toRational = toRational . sizedTo
instance Enum Word8 where
  { toEnum = sizedToEnum "Word8"; fromEnum = primIntegerToInt . sizedTo
  ; succ x = if x == maxBound then error "Enum.succ{Word8}: tried to take `succ' of maxBound" else x + 1
  ; pred x = if x == minBound then error "Enum.pred{Word8}: tried to take `pred' of minBound" else x - 1
  ; enumFrom x = sizedEnumFromTo x maxBound; enumFromTo = sizedEnumFromTo
  ; enumFromThen x y = sizedEnumFromThenTo x y (if y >= x then maxBound else minBound)
  ; enumFromThenTo = sizedEnumFromThenTo }
instance Integral Word8 where
  { quot = sized2 quot; rem = sized2 rem; div = sized2 div; mod = sized2 mod
  ; quotRem = sizedQuotRem; divMod = sizedDivMod; toInteger = sizedTo }
instance Ix Word8 where
  { range = uncurry sizedEnumFromTo; index (m, _) i = primIntegerToInt (sizedTo i - sizedTo m)
  ; inRange (m, n) i = m <= i && i <= n }
instance Bits Word8 where
  { (.&.) = sized2 primAnd; (.|.) = sized2 primOr; xor = sized2 primXor
  ; complement = sizedFrom . primComplement . sizedTo; shiftL a n = sizedFrom (primShiftL (sizedTo a) n)
  ; shiftR = sizedShiftR; testBit = sizedTestBit; popCount = sizedPopCount; bit n = sizedFrom (primShiftL 1 n)
  ; setBit a n = a .|. bit n; clearBit a n = a .&. complement (bit n); complementBit a n = a `xor` bit n
  ; zeroBits = 0 }

newtype Word16 = MkWord16 Integer deriving (Eq, Ord)
instance SizedInt Word16 where
  sizedBits _ = 16
  sizedSigned _ = False
  sizedTo (MkWord16 x) = x
  sizedRaw = MkWord16
instance Show Word16 where showsPrec = sizedShowsPrec
instance Read Word16 where readsPrec = sizedReadsPrec
instance Bounded Word16 where { minBound = sizedMin; maxBound = sizedMax }
instance Num Word16 where
  { (+) = sized2 (+); (-) = sized2 (-); (*) = sized2 (*); negate = sizedFrom . negate . sizedTo
  ; abs = sizedFrom . abs . sizedTo; signum = sizedFrom . signum . sizedTo; fromInteger = sizedFrom }
instance Real Word16 where toRational = toRational . sizedTo
instance Enum Word16 where
  { toEnum = sizedToEnum "Word16"; fromEnum = primIntegerToInt . sizedTo
  ; succ x = if x == maxBound then error "Enum.succ{Word16}: tried to take `succ' of maxBound" else x + 1
  ; pred x = if x == minBound then error "Enum.pred{Word16}: tried to take `pred' of minBound" else x - 1
  ; enumFrom x = sizedEnumFromTo x maxBound; enumFromTo = sizedEnumFromTo
  ; enumFromThen x y = sizedEnumFromThenTo x y (if y >= x then maxBound else minBound)
  ; enumFromThenTo = sizedEnumFromThenTo }
instance Integral Word16 where
  { quot = sized2 quot; rem = sized2 rem; div = sized2 div; mod = sized2 mod
  ; quotRem = sizedQuotRem; divMod = sizedDivMod; toInteger = sizedTo }
instance Ix Word16 where
  { range = uncurry sizedEnumFromTo; index (m, _) i = primIntegerToInt (sizedTo i - sizedTo m)
  ; inRange (m, n) i = m <= i && i <= n }
instance Bits Word16 where
  { (.&.) = sized2 primAnd; (.|.) = sized2 primOr; xor = sized2 primXor
  ; complement = sizedFrom . primComplement . sizedTo; shiftL a n = sizedFrom (primShiftL (sizedTo a) n)
  ; shiftR = sizedShiftR; testBit = sizedTestBit; popCount = sizedPopCount; bit n = sizedFrom (primShiftL 1 n)
  ; setBit a n = a .|. bit n; clearBit a n = a .&. complement (bit n); complementBit a n = a `xor` bit n
  ; zeroBits = 0 }

newtype Word32 = MkWord32 Integer deriving (Eq, Ord)
instance SizedInt Word32 where
  sizedBits _ = 32
  sizedSigned _ = False
  sizedTo (MkWord32 x) = x
  sizedRaw = MkWord32
instance Show Word32 where showsPrec = sizedShowsPrec
instance Read Word32 where readsPrec = sizedReadsPrec
instance Bounded Word32 where { minBound = sizedMin; maxBound = sizedMax }
instance Num Word32 where
  { (+) = sized2 (+); (-) = sized2 (-); (*) = sized2 (*); negate = sizedFrom . negate . sizedTo
  ; abs = sizedFrom . abs . sizedTo; signum = sizedFrom . signum . sizedTo; fromInteger = sizedFrom }
instance Real Word32 where toRational = toRational . sizedTo
instance Enum Word32 where
  { toEnum = sizedToEnum "Word32"; fromEnum = primIntegerToInt . sizedTo
  ; succ x = if x == maxBound then error "Enum.succ{Word32}: tried to take `succ' of maxBound" else x + 1
  ; pred x = if x == minBound then error "Enum.pred{Word32}: tried to take `pred' of minBound" else x - 1
  ; enumFrom x = sizedEnumFromTo x maxBound; enumFromTo = sizedEnumFromTo
  ; enumFromThen x y = sizedEnumFromThenTo x y (if y >= x then maxBound else minBound)
  ; enumFromThenTo = sizedEnumFromThenTo }
instance Integral Word32 where
  { quot = sized2 quot; rem = sized2 rem; div = sized2 div; mod = sized2 mod
  ; quotRem = sizedQuotRem; divMod = sizedDivMod; toInteger = sizedTo }
instance Ix Word32 where
  { range = uncurry sizedEnumFromTo; index (m, _) i = primIntegerToInt (sizedTo i - sizedTo m)
  ; inRange (m, n) i = m <= i && i <= n }
instance Bits Word32 where
  { (.&.) = sized2 primAnd; (.|.) = sized2 primOr; xor = sized2 primXor
  ; complement = sizedFrom . primComplement . sizedTo; shiftL a n = sizedFrom (primShiftL (sizedTo a) n)
  ; shiftR = sizedShiftR; testBit = sizedTestBit; popCount = sizedPopCount; bit n = sizedFrom (primShiftL 1 n)
  ; setBit a n = a .|. bit n; clearBit a n = a .&. complement (bit n); complementBit a n = a `xor` bit n
  ; zeroBits = 0 }

newtype Word64 = MkWord64 Integer deriving (Eq, Ord)
instance SizedInt Word64 where
  sizedBits _ = 64
  sizedSigned _ = False
  sizedTo (MkWord64 x) = x
  sizedRaw = MkWord64
instance Show Word64 where showsPrec = sizedShowsPrec
instance Read Word64 where readsPrec = sizedReadsPrec
instance Bounded Word64 where { minBound = sizedMin; maxBound = sizedMax }
instance Num Word64 where
  { (+) = sized2 (+); (-) = sized2 (-); (*) = sized2 (*); negate = sizedFrom . negate . sizedTo
  ; abs = sizedFrom . abs . sizedTo; signum = sizedFrom . signum . sizedTo; fromInteger = sizedFrom }
instance Real Word64 where toRational = toRational . sizedTo
instance Enum Word64 where
  { toEnum = sizedToEnum "Word64"; fromEnum = primIntegerToInt . sizedTo
  ; succ x = if x == maxBound then error "Enum.succ{Word64}: tried to take `succ' of maxBound" else x + 1
  ; pred x = if x == minBound then error "Enum.pred{Word64}: tried to take `pred' of minBound" else x - 1
  ; enumFrom x = sizedEnumFromTo x maxBound; enumFromTo = sizedEnumFromTo
  ; enumFromThen x y = sizedEnumFromThenTo x y (if y >= x then maxBound else minBound)
  ; enumFromThenTo = sizedEnumFromThenTo }
instance Integral Word64 where
  { quot = sized2 quot; rem = sized2 rem; div = sized2 div; mod = sized2 mod
  ; quotRem = sizedQuotRem; divMod = sizedDivMod; toInteger = sizedTo }
instance Ix Word64 where
  { range = uncurry sizedEnumFromTo; index (m, _) i = primIntegerToInt (sizedTo i - sizedTo m)
  ; inRange (m, n) i = m <= i && i <= n }
instance Bits Word64 where
  { (.&.) = sized2 primAnd; (.|.) = sized2 primOr; xor = sized2 primXor
  ; complement = sizedFrom . primComplement . sizedTo; shiftL a n = sizedFrom (primShiftL (sizedTo a) n)
  ; shiftR = sizedShiftR; testBit = sizedTestBit; popCount = sizedPopCount; bit n = sizedFrom (primShiftL 1 n)
  ; setBit a n = a .|. bit n; clearBit a n = a .&. complement (bit n); complementBit a n = a `xor` bit n
  ; zeroBits = 0 }

------------------------------------------------------------------------
-- Ix and arrays

class Ord a => Ix a where
  range :: (a, a) -> [a]
  index :: (a, a) -> a -> Int
  inRange :: (a, a) -> a -> Bool
  rangeSize :: (a, a) -> Int
  rangeSize b@(_, h) = if inRange b h then index b h + 1 else 0

instance Ix Int where
  range (m, n) = enumFromTo m n
  index b@(m, n) i = if inRange b i then i - m else indexError (show b) (show i)
  inRange (m, n) i = m <= i && i <= n

instance Ix Integer where
  range (m, n) = enumFromTo m n
  index b@(m, n) i = if inRange b i then primIntegerToInt (i - m) else indexError (show b) (show i)
  inRange (m, n) i = m <= i && i <= n

instance Ix Char where
  range (m, n) = enumFromTo m n
  index b@(m, n) i = if inRange b i then ord i - ord m else indexError (show b) (show i)
  inRange (m, n) i = m <= i && i <= n

instance Ix Bool where
  range (m, n) = enumFromTo m n
  index b@(m, n) i = if inRange b i then fromEnum i - fromEnum m else indexError (show b) (show i)
  inRange (m, n) i = m <= i && i <= n

instance (Ix a, Ix b) => Ix (a, b) where
  range ((l1, l2), (u1, u2)) = [(i1, i2) | i1 <- range (l1, u1), i2 <- range (l2, u2)]
  index ((l1, l2), (u1, u2)) (i1, i2) = index (l1, u1) i1 * rangeSize (l2, u2) + index (l2, u2) i2
  inRange ((l1, l2), (u1, u2)) (i1, i2) = inRange (l1, u1) i1 && inRange (l2, u2) i2

instance (Ix a, Ix b, Ix c) => Ix (a, b, c) where
  range ((l1, l2, l3), (u1, u2, u3)) =
    [(i1, i2, i3) | i1 <- range (l1, u1), i2 <- range (l2, u2), i3 <- range (l3, u3)]
  index ((l1, l2, l3), (u1, u2, u3)) (i1, i2, i3) =
    (index (l1, u1) i1 * rangeSize (l2, u2) + index (l2, u2) i2) * rangeSize (l3, u3) + index (l3, u3) i3
  inRange ((l1, l2, l3), (u1, u2, u3)) (i1, i2, i3) =
    inRange (l1, u1) i1 && inRange (l2, u2) i2 && inRange (l3, u3) i3

indexError :: String -> String -> a
indexError rng i = errorWithoutStackTrace ("Ix.index: index " ++ i ++ " out of range " ++ rng)

data Array i e = MkArray (i, i) Int (Vector e)

array :: Ix i => (i, i) -> [(i, e)] -> Array i e
array b ies = let n = rangeSize b
              in MkArray b n (primVectorUpdate (primVectorFromList n []) [(index b i, e) | (i, e) <- ies])

listArray :: Ix i => (i, i) -> [e] -> Array i e
listArray b es = let n = rangeSize b in MkArray b n (primVectorFromList n es)

(!) :: Ix i => Array i e -> i -> e
(!) (MkArray b _ v) i = primVectorRef v (index b i)

(!?.) :: Ix i => Array i e -> i -> Maybe e
(!?.) (MkArray b _ v) i = if inRange b i then Just (primVectorRef v (index b i)) else Nothing

bounds :: Array i e -> (i, i)
bounds (MkArray b _ _) = b

numElements :: Array i e -> Int
numElements (MkArray _ n _) = n

indices :: Ix i => Array i e -> [i]
indices (MkArray b _ _) = range b

elems :: Array i e -> [e]
elems (MkArray _ _ v) = primVectorElems v

assocs :: Ix i => Array i e -> [(i, e)]
assocs a = zip (indices a) (elems a)

(//) :: Ix i => Array i e -> [(i, e)] -> Array i e
(//) (MkArray b n v) ies = MkArray b n (primVectorUpdate v [(index b i, e) | (i, e) <- ies])

accumArray :: Ix i => (e -> a -> e) -> e -> (i, i) -> [(i, a)] -> Array i e
accumArray f z b ies = let n = rangeSize b
                       in MkArray b n (primVectorAccum f (primVectorFromList n (replicate n z))
                                                         [(index b i, x) | (i, x) <- ies])

accum :: Ix i => (e -> a -> e) -> Array i e -> [(i, a)] -> Array i e
accum f (MkArray b n v) ies = MkArray b n (primVectorAccum f v [(index b i, x) | (i, x) <- ies])

amap :: (a -> b) -> Array i a -> Array i b
amap f (MkArray b n v) = MkArray b n (primVectorFromList n (map f (primVectorElems v)))

ixmap :: (Ix i, Ix j) => (i, i) -> (i -> j) -> Array j e -> Array i e
ixmap b f a = listArray b [a ! f i | i <- range b]

instance Functor (Array i) where
  fmap = amap

instance Foldable (Array i) where
  foldr f z a = listFoldr f z (elems a)
  toList = elems
  length = numElements
  null a = numElements a == 0

instance (Ix i, Eq e) => Eq (Array i e) where
  a == b = bounds a == bounds b && elems a == elems b

instance (Ix i, Ord e) => Ord (Array i e) where
  compare a b = compare (assocs a) (assocs b)

instance (Ix i, Show i, Show e) => Show (Array i e) where
  showsPrec d a = showParen (d > 10) (showString "array " . showsPrec 11 (bounds a) . showChar ' ' . showsPrec 11 (assocs a))

------------------------------------------------------------------------
-- IsString (for the OverloadedStrings extension)

class IsString a where
  fromString :: String -> a

instance IsString [c] where
  fromString s = primUnsafeCoerce s
