-- Shortest path through a maze with breadth-first search.
-- Run:  husk examples/grid_bfs.hs < examples/maze.txt
import qualified Data.Map.Strict as Map

type Pos = (Int, Int)

parseGrid :: String -> Map.Map Pos Char
parseGrid input = Map.fromList
  [ ((r, c), ch) | (r, row) <- zip [0 ..] (lines input), (c, ch) <- zip [0 ..] row ]

neighbours :: Map.Map Pos Char -> Pos -> [Pos]
neighbours grid (r, c) =
  [ p | p <- [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]
      , Map.findWithDefault '#' p grid /= '#' ]

-- BFS returning the distance to every reachable position
bfs :: Map.Map Pos Char -> Pos -> Map.Map Pos Int
bfs grid start = go (Map.singleton start 0) [start] []
  where
    go dist [] [] = dist
    go dist [] next = go dist (reverse next) []
    go dist (p:queue) next =
      let d = dist Map.! p
          new = [ q | q <- neighbours grid p, not (Map.member q dist) ]
          dist' = foldr (\q m -> Map.insert q (d + 1) m) dist new
      in go dist' queue (reverse new ++ next)

main :: IO ()
main = do
  input <- getContents
  let grid = parseGrid input
      find ch = head [ p | (p, x) <- Map.toList grid, x == ch ]
      start = find 'S'
      goal = find 'E'
      dist = bfs grid start
  putStrLn ("Shortest path from S to E: " ++ maybe "unreachable" show (Map.lookup goal dist))
  putStrLn ("Reachable cells: " ++ show (Map.size dist))
