-- Conway's Game of Life on an unbounded board, using a Set of live cells.
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

type Cell = (Int, Int)

step :: Set.Set Cell -> Set.Set Cell
step live = Set.fromList [c | (c, n) <- Map.toList counts, n == 3 || (n == 2 && c `Set.member` live)]
  where
    counts = Map.fromListWith (+)
      [((x + dx, y + dy), 1 :: Int) | (x, y) <- Set.toList live
                                     , dx <- [-1, 0, 1], dy <- [-1, 0, 1], (dx, dy) /= (0, 0)]

render :: Set.Set Cell -> String
render live = unlines [[if (x, y) `Set.member` live then '#' else '.' | x <- [0 .. 9]] | y <- [0 .. 5]]

glider :: Set.Set Cell
glider = Set.fromList [(1, 0), (2, 1), (0, 2), (1, 2), (2, 2)]

main :: IO ()
main = do
  let generations = iterate step glider
  mapM_ (\(i, g) -> putStrLn ("generation " ++ show i) >> putStr (render g)) (zip [0 :: Int, 4, 8] [generations !! 0, generations !! 4, generations !! 8])
  print (Set.size (iterate step glider !! 1000))
