-- Count lines, words and characters on standard input, and show the most
-- frequent words.   husk examples/wordcount.hs < README.md
import qualified Data.Map.Strict as Map
import Data.Char (toLower, isAlpha)
import Data.List (sortBy)
import Data.Ord (comparing, Down(..))
import Text.Printf (printf)

main :: IO ()
main = interact $ \input ->
  let ws = words (map toLower (filter (\c -> isAlpha c || c == ' ' || c == '\n') input))
      freq = Map.fromListWith (+) [(w, 1 :: Int) | w <- ws, length w > 3]
      top = take 5 (sortBy (comparing (Down . snd)) (Map.toList freq))
  in printf "%d lines, %d words, %d characters\n" (length (lines input)) (length (words input)) (length input)
     ++ concat [printf "%-12s %d\n" w n | (w, n) <- top]
