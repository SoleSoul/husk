-- Word frequencies with Data.Map
import qualified Data.Map.Strict as Map
import Data.List (sortBy)
import Data.Ord (comparing, Down(..))

main :: IO ()
main = do
  let ws = [ [c1, c2] | i <- [1 .. 200000 :: Int], let c1 = toEnum (97 + i `mod` 26) :: Char, let c2 = toEnum (97 + (i * 7) `mod` 13) ]
      freq = Map.fromListWith (+) [(w, 1 :: Int) | w <- ws]
  print (Map.size freq)
  mapM_ print (take 5 (sortBy (comparing (Down . snd)) (Map.toList freq)))
