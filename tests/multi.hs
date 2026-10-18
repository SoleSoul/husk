import Geometry.Shapes
import Utils (totalArea, Pretty(..))
import qualified Utils as U

main :: IO ()
main = do
  let shapes = [Circle 1, Square 2, unitSquare]
  print shapes
  print (totalArea shapes, U.totalArea [unitSquare])
  mapM_ (putStrLn . pretty) shapes
