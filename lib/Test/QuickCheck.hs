-- Test.QuickCheck: property-based testing, a subset of the QuickCheck package.
--
--   prop_reverse :: [Int] -> Bool
--   prop_reverse xs = reverse (reverse xs) == xs
--   main = quickCheck prop_reverse        -- +++ OK, passed 100 tests.
--
-- Failing properties are shrunk to a small counterexample, as in QuickCheck.
-- The random values differ from GHC's QuickCheck, so a failing property may
-- report a different (but equally valid) counterexample.
module Test.QuickCheck (
  -- running tests
  quickCheck, quickCheckWith, verboseCheck, verboseCheckWith, quickCheckResult,
  quickCheckWithResult,
  Args(..), stdArgs, Result(..), isSuccess,
  -- properties
  Testable(..), Property, (==>), (===), (=/=), (.&&.), (.||.), conjoin, disjoin,
  counterexample, label, collect, classify, withMaxSuccess, once, forAll, forAllShrink,
  ioProperty, discard, expectFailure,
  -- generators
  Gen, Arbitrary(..), CoArbitrary(..), choose, chooseInt, elements, oneof, frequency,
  sized, getSize, resize, scale, listOf, listOf1, vectorOf, infiniteListOf, suchThat,
  suchThatMaybe, sublistOf, shuffle, growingElements, generate, sample, sample', variant,
  arbitrarySizedIntegral, arbitraryBoundedIntegral, arbitraryBoundedEnum,
  arbitrarySizedFractional, arbitraryPrintableChar,
  shrinkList, shrinkIntegral, shrinkNothing, shrinkMap,
  -- modifiers
  Positive(..), NonNegative(..), NonZero(..), NonEmptyList(..), OrderedList(..), Small(..),
  Blind(..), Fun, applyFun
  ) where

import Data.List (sort, sortBy, nub, group, intercalate)
import Data.Char (isUpper, isLower, isDigit, isSpace, toLower)
import Data.Ratio (Ratio, (%))
import Data.Word
import Data.Int
import System.Random
import Control.Exception
import System.IO.Unsafe (unsafePerformIO)

infixr 0 ==>
infix 4 ===, =/=
infixr 1 .&&.
infixr 1 .||.

------------------------------------------------------------------------
-- Generators

-- a generator gets a random number generator and a size
newtype Gen a = MkGen (StdGen -> Int -> a)

unGen :: Gen a -> StdGen -> Int -> a
unGen (MkGen f) = f

instance Functor Gen where
  fmap f (MkGen h) = MkGen (\g n -> f (h g n))

instance Applicative Gen where
  pure x = MkGen (\_ _ -> x)
  mf <*> mx = mf >>= \f -> fmap f mx

instance Monad Gen where
  MkGen m >>= k = MkGen (\g n -> let (g1, g2) = split g in unGen (k (m g1 n)) g2 n)

choose :: Random a => (a, a) -> Gen a
choose r = MkGen (\g _ -> fst (randomR r g))

chooseInt :: (Int, Int) -> Gen Int
chooseInt = choose

sized :: (Int -> Gen a) -> Gen a
sized f = MkGen (\g n -> unGen (f n) g n)

getSize :: Gen Int
getSize = sized pure

resize :: Int -> Gen a -> Gen a
resize n (MkGen f) = MkGen (\g _ -> f g n)

scale :: (Int -> Int) -> Gen a -> Gen a
scale f g = sized (\n -> resize (f n) g)

elements :: [a] -> Gen a
elements [] = error "QuickCheck.elements used with empty list"
elements xs = (xs !!) <$> choose (0, length xs - 1)

growingElements :: [a] -> Gen a
growingElements [] = error "QuickCheck.growingElements used with empty list"
growingElements xs = sized (\n -> elements (take (max 1 (min n (length xs))) xs))

oneof :: [Gen a] -> Gen a
oneof [] = error "QuickCheck.oneof used with empty list"
oneof gs = choose (0, length gs - 1) >>= (gs !!)

frequency :: [(Int, Gen a)] -> Gen a
frequency [] = error "QuickCheck.frequency used with empty list"
frequency xs = choose (1, sum (map fst xs)) >>= pick xs
  where pick ((k, g) : rest) n = if n <= k then g else pick rest (n - k)
        pick [] _ = error "QuickCheck.frequency: no generator"

listOf :: Gen a -> Gen [a]
listOf g = sized (\n -> choose (0, n) >>= \k -> vectorOf k g)

listOf1 :: Gen a -> Gen [a]
listOf1 g = sized (\n -> choose (1, max 1 n) >>= \k -> vectorOf k g)

vectorOf :: Int -> Gen a -> Gen [a]
vectorOf k g = sequence (replicate k g)

infiniteListOf :: Gen a -> Gen [a]
infiniteListOf g = MkGen (\gen n -> [unGen g g' n | g' <- gens gen])
  where gens gen = let (a, b) = split gen in a : gens b

suchThat :: Gen a -> (a -> Bool) -> Gen a
suchThat g p = do
  mx <- suchThatMaybe g p
  case mx of
    Just x -> return x
    Nothing -> sized (\n -> resize (n + 1) (suchThat g p))

suchThatMaybe :: Gen a -> (a -> Bool) -> Gen (Maybe a)
suchThatMaybe g p = sized (\n -> try n (2 * max 1 n))
  where
    try m k
      | m > k = return Nothing
      | otherwise = do
          x <- resize m g
          if p x then return (Just x) else try (m + 1) k

sublistOf :: [a] -> Gen [a]
sublistOf xs = fmap (map fst . filter snd . zip xs) (vectorOf (length xs) (choose (False, True)))

shuffle :: [a] -> Gen [a]
shuffle xs = do
  keys <- vectorOf (length xs) (choose (0, 1 :: Double))
  return (map snd (sortBy (\a b -> compare (fst a) (fst b)) (zip keys xs)))

-- a different generator for every integer (used to make random functions)
variant :: Integral n => n -> Gen a -> Gen a
variant k (MkGen f) = MkGen (\g n -> f (vary (toInteger k) g) n)
  where
    vary m g = foldl (\h b -> (if b then fst else snd) (split h)) g (bits m)
    bits m = (m < 0) : go (abs m)
    go 0 = [False]
    go m = True : odd m : go (m `div` 2)

-- a value drawn from the generator, with a fresh seed
generate :: Gen a -> IO a
generate g = do
  gen <- newStdGen
  return (unGen g gen 30)

sample' :: Gen a -> IO [a]
sample' g = do
  gen <- newStdGen
  let gens = take 11 (iterate (snd . split) gen)
  return [unGen g s n | (s, n) <- zip gens [0, 2 .. 20]]

sample :: Show a => Gen a -> IO ()
sample g = sample' g >>= mapM_ print

------------------------------------------------------------------------
-- Arbitrary

class Arbitrary a where
  arbitrary :: Gen a
  shrink :: a -> [a]
  shrink _ = []

arbitrarySizedIntegral :: Integral a => Gen a
arbitrarySizedIntegral = sized (\n -> fromInteger <$> choose (negate (toInteger n), toInteger n))

arbitraryBoundedIntegral :: (Bounded a, Integral a) => Gen a
arbitraryBoundedIntegral = r
  where r = fromInteger <$> choose (toInteger (minBound `asTypeOf` unGenValue r),
                                    toInteger (maxBound `asTypeOf` unGenValue r))
        unGenValue :: Gen a -> a
        unGenValue _ = undefined

arbitraryBoundedEnum :: (Bounded a, Enum a) => Gen a
arbitraryBoundedEnum = r
  where r = toEnum <$> choose (fromEnum (minBound `asTypeOf` unGenValue r),
                               fromEnum (maxBound `asTypeOf` unGenValue r))
        unGenValue :: Gen a -> a
        unGenValue _ = undefined

arbitrarySizedFractional :: Fractional a => Gen a
arbitrarySizedFractional = sized $ \n -> do
  let n' = toInteger n
  b <- choose (1, 9999999999)
  a <- choose (negate n' * b, n' * b)
  return (fromRational (a % b))

arbitraryPrintableChar :: Gen Char
arbitraryPrintableChar = choose (' ', '~')

shrinkNothing :: a -> [a]
shrinkNothing _ = []

shrinkMap :: Arbitrary a => (a -> b) -> (b -> a) -> b -> [b]
shrinkMap f g = map f . shrink . g

shrinkIntegral :: Integral a => a -> [a]
shrinkIntegral x =
  nub ([negate x | x < 0, negate x > x] ++
       [x' | x' <- takeWhile (<< x) (0 : [x - i | i <- tail (iterate (`quot` 2) x)])])
  where
    a << b = case (a >= 0, b >= 0) of
      (True, True) -> a < b
      (False, False) -> a > b
      (True, False) -> a + b < 0
      (False, True) -> a + b > 0

shrinkFractional :: RealFrac a => a -> [a]
shrinkFractional x =
  nub ([negate x | x < 0] ++ [0 | x /= 0] ++
       [fromInteger t | let t = truncate x, fromInteger t /= x, abs (fromInteger t) < abs x])

shrinkList :: (a -> [a]) -> [a] -> [[a]]
shrinkList shr xs = concat [removes k n xs | k <- takeWhile (> 0) (iterate (`div` 2) n)] ++ shrinkOne xs
  where
    n = length xs
    shrinkOne [] = []
    shrinkOne (y : ys) = [y' : ys | y' <- shr y] ++ [y : ys' | ys' <- shrinkOne ys]
    removes k m ys
      | k > m = []
      | null ys2 = [[]]
      | otherwise = ys2 : map (ys1 ++) (removes k (m - k) ys2)
      where ys1 = take k ys
            ys2 = drop k ys

instance Arbitrary () where
  arbitrary = return ()

instance Arbitrary Bool where
  arbitrary = choose (False, True)
  shrink True = [False]
  shrink False = []

instance Arbitrary Ordering where
  arbitrary = elements [LT, EQ, GT]
  shrink GT = [LT, EQ]
  shrink EQ = [LT]
  shrink LT = []

instance Arbitrary Int where
  arbitrary = arbitrarySizedIntegral
  shrink = shrinkIntegral

instance Arbitrary Integer where
  arbitrary = arbitrarySizedIntegral
  shrink = shrinkIntegral

instance Arbitrary Word where
  arbitrary = sized (\n -> fromIntegral <$> choose (0, n))
  shrink = shrinkIntegral

instance Arbitrary Word8 where
  arbitrary = arbitraryBoundedIntegral
  shrink = shrinkIntegral

instance Arbitrary Int8 where
  arbitrary = arbitraryBoundedIntegral
  shrink = shrinkIntegral

instance Arbitrary Char where
  arbitrary = frequency [(3, choose ('a', 'z')), (1, arbitraryPrintableChar)]
  -- QuickCheck's order: lower case, upper case, digits, space, others
  shrink c = filter (\c' -> stamp c' < stamp c)
                    (nub ("abc" ++ [toLower c | isUpper c] ++ "ABC123 \n"))
    where stamp a = ((not (isLower a), not (isUpper a), not (isDigit a)), (a /= ' ', not (isSpace a), a))

instance Arbitrary Double where
  arbitrary = arbitrarySizedFractional
  shrink = shrinkFractional

instance Arbitrary Float where
  arbitrary = arbitrarySizedFractional
  shrink = shrinkFractional

instance (Integral a, Arbitrary a) => Arbitrary (Ratio a) where
  arbitrary = do
    n <- arbitrary
    d <- arbitrary `suchThat` (/= 0)
    return (if d < 0 then negate n % negate d else n % d)

instance Arbitrary a => Arbitrary [a] where
  arbitrary = listOf arbitrary
  shrink = shrinkList shrink

instance Arbitrary a => Arbitrary (Maybe a) where
  arbitrary = frequency [(1, return Nothing), (3, Just <$> arbitrary)]
  shrink Nothing = []
  shrink (Just x) = Nothing : map Just (shrink x)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Either a b) where
  arbitrary = oneof [Left <$> arbitrary, Right <$> arbitrary]
  shrink (Left x) = map Left (shrink x)
  shrink (Right y) = map Right (shrink y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (a, b) where
  arbitrary = (,) <$> arbitrary <*> arbitrary
  shrink (a, b) = [(a', b) | a' <- shrink a] ++ [(a, b') | b' <- shrink b]

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (a, b, c) where
  arbitrary = (,,) <$> arbitrary <*> arbitrary <*> arbitrary
  shrink (a, b, c) = [(a', b', c') | (a', (b', c')) <- shrink (a, (b, c))]

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (a, b, c, d) where
  arbitrary = (,,,) <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink (a, b, c, d) = [(a', b', c', d') | (a', (b', (c', d'))) <- shrink (a, (b, (c, d)))]

-- random functions
instance (CoArbitrary a, Arbitrary b) => Arbitrary (a -> b) where
  arbitrary = MkGen (\g n -> \x -> unGen (coarbitrary x arbitrary) g n)

class CoArbitrary a where
  coarbitrary :: a -> Gen b -> Gen b

instance CoArbitrary () where
  coarbitrary _ = id

instance CoArbitrary Bool where
  coarbitrary b = variant (if b then 1 else 0 :: Int)

instance CoArbitrary Ordering where
  coarbitrary o = variant (fromEnum o)

instance CoArbitrary Int where
  coarbitrary = variant

instance CoArbitrary Integer where
  coarbitrary = variant

instance CoArbitrary Char where
  coarbitrary c = variant (fromEnum c)

instance CoArbitrary Double where
  coarbitrary x = let (m, e) = decodeFloat x in variant m . variant e

instance CoArbitrary a => CoArbitrary [a] where
  coarbitrary [] = variant (0 :: Int)
  coarbitrary (x : xs) = variant (1 :: Int) . coarbitrary x . coarbitrary xs

instance CoArbitrary a => CoArbitrary (Maybe a) where
  coarbitrary Nothing = variant (0 :: Int)
  coarbitrary (Just x) = variant (1 :: Int) . coarbitrary x

instance (CoArbitrary a, CoArbitrary b) => CoArbitrary (Either a b) where
  coarbitrary (Left x) = variant (0 :: Int) . coarbitrary x
  coarbitrary (Right y) = variant (1 :: Int) . coarbitrary y

instance (CoArbitrary a, CoArbitrary b) => CoArbitrary (a, b) where
  coarbitrary (a, b) = coarbitrary a . coarbitrary b

------------------------------------------------------------------------
-- Modifiers

newtype Positive a = Positive { getPositive :: a } deriving (Eq, Ord, Show)
newtype NonNegative a = NonNegative { getNonNegative :: a } deriving (Eq, Ord, Show)
newtype NonZero a = NonZero { getNonZero :: a } deriving (Eq, Ord, Show)
newtype NonEmptyList a = NonEmpty { getNonEmpty :: [a] } deriving (Eq, Ord, Show)
newtype OrderedList a = Ordered { getOrdered :: [a] } deriving (Eq, Ord, Show)
newtype Small a = Small { getSmall :: a } deriving (Eq, Ord, Show)

-- a value that is not shown in counterexamples
newtype Blind a = Blind { getBlind :: a }

instance Show (Blind a) where
  show _ = "(*)"

instance (Num a, Ord a, Arbitrary a) => Arbitrary (Positive a) where
  arbitrary = (Positive . abs) <$> (arbitrary `suchThat` (/= 0))
  shrink (Positive x) = [Positive x' | x' <- shrink x, x' > 0]

instance (Num a, Ord a, Arbitrary a) => Arbitrary (NonNegative a) where
  arbitrary = (NonNegative . abs) <$> arbitrary
  shrink (NonNegative x) = [NonNegative x' | x' <- shrink x, x' >= 0]

instance (Num a, Eq a, Arbitrary a) => Arbitrary (NonZero a) where
  arbitrary = NonZero <$> (arbitrary `suchThat` (/= 0))
  shrink (NonZero x) = [NonZero x' | x' <- shrink x, x' /= 0]

instance Arbitrary a => Arbitrary (NonEmptyList a) where
  arbitrary = NonEmpty <$> listOf1 arbitrary
  shrink (NonEmpty xs) = [NonEmpty xs' | xs' <- shrink xs, not (null xs')]

instance (Ord a, Arbitrary a) => Arbitrary (OrderedList a) where
  arbitrary = (Ordered . sort) <$> arbitrary
  shrink (Ordered xs) = [Ordered xs' | xs' <- map sort (shrink xs)]

instance Integral a => Arbitrary (Small a) where
  arbitrary = Small <$> sized (\n -> let m = toInteger (min n 10) in fromInteger <$> choose (negate m, m))
  shrink (Small x) = map Small (shrinkIntegral x)

instance Arbitrary a => Arbitrary (Blind a) where
  arbitrary = Blind <$> arbitrary
  shrink (Blind x) = map Blind (shrink x)

-- a random function that can be shown (as <fun>); use applyFun to call it
newtype Fun a b = MkFun (a -> b)

instance Show (Fun a b) where
  show _ = "<fun>"

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Fun a b) where
  arbitrary = MkFun <$> arbitrary

applyFun :: Fun a b -> a -> b
applyFun (MkFun f) = f

------------------------------------------------------------------------
-- Properties
--
-- Running a property gives a rose tree of results: the result for the
-- generated arguments, and below it the results for shrunk arguments.

data Rose a = MkRose a [Rose a]

instance Functor Rose where
  fmap f (MkRose x rs) = MkRose (f x) (map (fmap f) rs)

data TestResult = TestResult
  { ok :: Maybe Bool           -- Nothing: discarded
  , failMsg :: Maybe String    -- an exception
  , testArgs :: [String]       -- shown arguments and counterexample lines
  , testLabels :: [String]
  , expectFail :: Bool
  , maxTests :: Maybe Int
  , exhaustive :: Bool          -- no random input: one test is enough
  }

result :: Maybe Bool -> TestResult
result b = TestResult b Nothing [] [] False Nothing True

newtype Property = MkProperty (Gen (Rose TestResult))

unProperty :: Property -> Gen (Rose TestResult)
unProperty (MkProperty g) = g

mapResult :: Testable prop => (TestResult -> TestResult) -> prop -> Property
mapResult f p = MkProperty (fmap (fmap f) (unProperty (property p)))

-- evaluate a Bool, turning an exception into a failure
protect :: Bool -> TestResult
protect b = unsafePerformIO $ do
  r <- try (evaluate b)
  return $ case r of
    Right v -> result (Just v)
    Left e -> (result (Just False)) { failMsg = Just (show (e :: SomeException)) }

class Testable prop where
  property :: prop -> Property

instance Testable Bool where
  property b = MkProperty (return (MkRose (protect b) []))

instance Testable () where
  property () = property True

instance Testable Property where
  property p = p

instance Testable prop => Testable (Gen prop) where
  property g = MkProperty (g >>= unProperty . property)

instance (Arbitrary a, Show a, Testable prop) => Testable (a -> prop) where
  property f = forAllShrink arbitrary shrink f

forAll :: (Show a, Testable prop) => Gen a -> (a -> prop) -> Property
forAll g = forAllShrink g (const [])

forAllShrink :: (Show a, Testable prop) => Gen a -> (a -> [a]) -> (a -> prop) -> Property
forAllShrink gen shr pf = MkProperty (gen >>= \x -> MkGen (\g n -> rose g n x))
  where
    rose g n x =
      let MkRose r inner = unGen (unProperty (property (pf x))) g n
          addArg t = t { testArgs = show x : testArgs t, exhaustive = False }
      in MkRose (addArg r) ([rose g n x' | x' <- shr x] ++ map (fmap addArg) inner)

(==>) :: Testable prop => Bool -> prop -> Property
False ==> _ = MkProperty (return (MkRose (result Nothing) []))
True ==> p = property p

(===) :: (Eq a, Show a) => a -> a -> Property
x === y = counterexample (show x ++ (if same then " == " else " /= ") ++ show y) same
  where same = x == y

(=/=) :: (Eq a, Show a) => a -> a -> Property
x =/= y = counterexample (show x ++ (if diff then " /= " else " == ") ++ show y) diff
  where diff = x /= y

counterexample :: Testable prop => String -> prop -> Property
counterexample s = mapResult (\t -> t { testArgs = testArgs t ++ [s] })

label :: Testable prop => String -> prop -> Property
label s = mapResult (\t -> t { testLabels = s : testLabels t })

collect :: (Show a, Testable prop) => a -> prop -> Property
collect x = label (show x)

classify :: Testable prop => Bool -> String -> prop -> Property
classify True s p = label s p
classify False _ p = property p

withMaxSuccess :: Testable prop => Int -> prop -> Property
withMaxSuccess n = mapResult (\t -> t { maxTests = Just n })

once :: Testable prop => prop -> Property
once = withMaxSuccess 1

expectFailure :: Testable prop => prop -> Property
expectFailure = mapResult (\t -> t { expectFail = True })

discard :: a
discard = error "QuickCheck.discard"

ioProperty :: Testable prop => IO prop -> Property
ioProperty io = MkProperty (MkGen (\g n -> unGen (unProperty (property (unsafePerformIO io))) g n))

(.&&.) :: (Testable p, Testable q) => p -> q -> Property
p .&&. q = conjoin [property p, property q]

(.||.) :: (Testable p, Testable q) => p -> q -> Property
p .||. q = disjoin [property p, property q]

conjoin :: Testable prop => [prop] -> Property
conjoin ps = MkProperty $ do
  roses <- mapM (unProperty . property) ps
  return (combine roses)
  where
    combine roses = case [r | r@(MkRose t _) <- roses, ok t == Just False] of
      (MkRose t rs : _) -> MkRose t rs
      [] -> MkRose (if any (\(MkRose t _) -> ok t == Nothing) roses then result Nothing
                    else (result (Just True)) { testLabels = concat [testLabels t | MkRose t _ <- roses]
                                              , exhaustive = and [exhaustive t | MkRose t _ <- roses] }) []

disjoin :: Testable prop => [prop] -> Property
disjoin ps = MkProperty $ do
  roses <- mapM (unProperty . property) ps
  return $ case [r | r@(MkRose t _) <- roses, ok t == Just True] of
    (r : _) -> r
    [] -> MkRose ((result (Just False)) { testArgs = concat [testArgs t | MkRose t _ <- roses] }) []

------------------------------------------------------------------------
-- Running

data Args = Args { maxSuccess :: Int, maxDiscardRatio :: Int, maxSize :: Int, chatty :: Bool, maxShrinks :: Int }
  deriving Show

stdArgs :: Args
stdArgs = Args { maxSuccess = 100, maxDiscardRatio = 10, maxSize = 100, chatty = True, maxShrinks = 1000 }

data Result
  = Success { numTests :: Int, output :: String }
  | GaveUp { numTests :: Int, output :: String }
  | Failure { numTests :: Int, numShrinks :: Int, output :: String }
  deriving Show

isSuccess :: Result -> Bool
isSuccess (Success _ _) = True
isSuccess _ = False

quickCheck :: Testable prop => prop -> IO ()
quickCheck p = quickCheckWith stdArgs p

quickCheckWith :: Testable prop => Args -> prop -> IO ()
quickCheckWith args p = runTests args False p >> return ()

quickCheckResult :: Testable prop => prop -> IO Result
quickCheckResult = quickCheckWithResult stdArgs

-- (with chatty = False, nothing is printed)
quickCheckWithResult :: Testable prop => Args -> prop -> IO Result
quickCheckWithResult args = runTests args False

verboseCheck :: Testable prop => prop -> IO ()
verboseCheck = verboseCheckWith stdArgs

verboseCheckWith :: Testable prop => Args -> prop -> IO ()
verboseCheckWith args p = runTests args True p >> return ()

runTests :: Testable prop => Args -> Bool -> prop -> IO Result
runTests args verbose p = newStdGen >>= loop 0 0 []
  where
    prop = unProperty (property p)
    size n d = min (maxSize args) (n `mod` maxSize args + d `div` 10)
    loop n d labels gen = do
      let (g1, g2) = split gen
          MkRose t rs = unGen prop g1 (size n d)
          limit = maybe (maxSuccess args) id (maxTests t)
      if n >= limit then finish (Success n (successText n d labels))
      else if d >= maxDiscardRatio args * limit
        then finish (GaveUp n ("*** Gave up! Passed only " ++ plural n "test" ++ "; " ++ show d ++ " discarded tests.\n"))
      else case ok t of
        Nothing -> loop n (d + 1) labels g2
        Just True -> do
          if verbose then putStr ("Passed:\n" ++ unlines (testArgs t) ++ "\n") else return ()
          if exhaustive t
            then finish (Success (n + 1) (successText (n + 1) d (testLabels t : labels)))
            else loop (n + 1) d (testLabels t : labels) g2
        Just False
          | expectFail t -> finish (Success (n + 1) "+++ OK, failed as expected.\n")
          | otherwise -> do
              let (k, t') = shrinkLoop 0 t rs
              finish (Failure (n + 1) k (failureText (n + 1) k t'))
    finish r = (if chatty args then putStr (output r) else return ()) >> return r
    shrinkLoop k t rs
      | k >= maxShrinks args = (k, t)
      | otherwise = case [r | r@(MkRose t' _) <- rs, ok t' == Just False] of
          (MkRose t' rs' : _) -> shrinkLoop (k + 1) t' rs'
          [] -> (k, t)

failureText :: Int -> Int -> TestResult -> String
failureText n k t =
  "*** Failed! " ++ maybe "Falsified" (\m -> "Exception: '" ++ m ++ "'") (failMsg t)
  ++ " (after " ++ plural n "test" ++ (if k > 0 then " and " ++ plural k "shrink" else "") ++ "):\n"
  ++ unlines (testArgs t)

successText :: Int -> Int -> [[String]] -> String
successText n d labels
  | null tables = "+++ OK, passed " ++ plural n "test" ++ discarded ++ ".\n"
  | otherwise = "+++ OK, passed " ++ plural n "test" ++ discarded ++ ":\n" ++ unlines tables
  where
    discarded = if d > 0 then "; " ++ show d ++ " discarded" else ""
    names = [intercalate ", " ls | ls <- labels, not (null ls)]
    counts = sortBy (\a b -> compare (fst b) (fst a)) [(length g, head g) | g <- group (sort names)]
    tables = [show ((c * 100) `div` n) ++ "% " ++ s | (c, s) <- counts]

plural :: Int -> String -> String
plural 1 w = "1 " ++ w
plural k w = show k ++ " " ++ w ++ "s"
