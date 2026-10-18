-- 0/1 knapsack by dynamic programming.  The table is a lazy array whose
-- cells are defined in terms of other cells: each is computed on demand.
import Data.Array

knapsack :: Int -> [(Int, Int)] -> Int
knapsack capacity items = table ! (n, capacity)
  where
    n = length items
    itemArr = listArray (1, n) items
    table = array ((0, 0), (n, capacity))
      [((i, w), best i w) | i <- [0 .. n], w <- [0 .. capacity]]
    best 0 _ = 0
    best i w =
      let (weight, value) = itemArr ! i
          skip = table ! (i - 1, w)
      in if weight > w then skip else max skip (value + table ! (i - 1, w - weight))

main :: IO ()
main = do
  let items = [(12, 4), (2, 2), (1, 1), (1, 2), (4, 10)]   -- (weight, value)
  print (knapsack 15 items)
  let many = [(w, v) | i <- [1 .. 100], let w = 1 + i * 7 `mod` 23, let v = 1 + i * 13 `mod` 37]
  print (knapsack 500 many)
