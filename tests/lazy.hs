import Data.List (foldl')

-- knot-tying and infinite structures
ones :: [Int]
ones = 1 : ones

nats :: [Integer]
nats = [0 ..]

hammings :: [Integer]
hammings = 1 : merge3 (map (* 2) hammings) (map (* 3) hammings) (map (* 5) hammings)
  where
    merge3 a b c = merge a (merge b c)
    merge xx@(x:xs) yy@(y:ys)
      | x < y = x : merge xs yy
      | x > y = y : merge xx ys
      | otherwise = x : merge xs ys

collatz :: Int -> Int
collatz n = go n 0
  where go 1 !acc = acc
        go m !acc = go (if even m then m `div` 2 else 3 * m + 1) (acc + 1)

-- lazy evaluation: the error is never forced
lazyPair :: (Int, Int)
lazyPair = (1, error "never evaluated")

data Stream a = Cons a (Stream a)

sfrom :: Int -> Stream Int
sfrom n = Cons n (sfrom (n + 1))

stake :: Int -> Stream a -> [a]
stake 0 _ = []
stake n (Cons x xs) = x : stake (n - 1) xs

main :: IO ()
main = do
  print (take 5 ones)
  print (takeWhile (< 10) (map (* 2) nats))
  print (take 20 hammings)
  print (fst lazyPair)
  print (stake 5 (sfrom 10))
  -- deep recursion (non-tail) is fine: the Scheme stack grows as needed
  print (foldr (+) 0 [1 .. 1000000 :: Int])
  print (length (show (product [1 .. 3000 :: Integer])))
  print (sum [1 .. 3000000 :: Int])
  print (foldl' max 0 (map collatz [1 .. 30000]))
  let xs = [1 ..] :: [Int]
  print (head (dropWhile (< 1000000) xs))
  print (let fibs = 0 : 1 : zipWith (+) fibs (tail fibs) in fibs !! 300 :: Integer)
  print (length (filter even (concatMap (\x -> [x, x + 1]) [1 .. 100000 :: Int])))
  print (zip "abc" (cycle [True, False]))
  print (snd (foldl (\(a, b) x -> (a + x, b * 2)) (0, 1) [1 .. 10 :: Integer]))
