-- Longest common subsequence by dynamic programming over a lazy array:
-- the table's cells refer to each other and are evaluated on demand.
import Data.Array

lcs :: String -> String -> Int
lcs xs ys = table ! (0, 0)
  where
    n = length xs
    m = length ys
    xa = listArray (0, n - 1) xs
    ya = listArray (0, m - 1) ys
    table = array ((0, 0), (n, m)) [((i, j), f i j) | i <- [0 .. n], j <- [0 .. m]]
    f i j
      | i == n || j == m = 0
      | xa ! i == ya ! j = 1 + table ! (i + 1, j + 1)
      | otherwise = max (table ! (i + 1, j)) (table ! (i, j + 1))

main :: IO ()
main = do
  let a = take 600 (cycle "ACGTTGCAAG")
      b = take 600 (cycle "TGCATGCCGA")
  print (lcs a b)
