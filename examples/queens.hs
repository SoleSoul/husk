-- N-queens: count all solutions with the list monad
queens :: Int -> [[Int]]
queens n = go n
  where
    go 0 = [[]]
    go k = [q : qs | qs <- go (k - 1), q <- [1 .. n], safe q qs]
    safe q qs = and [q /= c && abs (q - c) /= d | (d, c) <- zip [1 ..] qs]

main :: IO ()
main = do
  print (length (queens 8))
  print (head (queens 10))
