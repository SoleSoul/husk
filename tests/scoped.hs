{-# LANGUAGE ScopedTypeVariables #-}
import Data.List (sortBy)

-- the local signature refers to the outer 'a'
pairsWith :: forall a. Ord a => [a] -> [(a, a)]
pairsWith xs = go xs
  where
    go :: [a] -> [(a, a)]
    go (x:y:rest) = (min x y, max x y) : go rest
    go _ = []

-- an annotation mentioning the outer type variable
sizeOfEmpty :: forall a. Show a => [a] -> String
sizeOfEmpty xs = show (length (filter (const True) xs :: [a]))

largest :: forall k v. (Ord v) => [(k, v)] -> Maybe k
largest [] = Nothing
largest kvs = Just (fst (head sorted))
  where sorted :: [(k, v)]
        sorted = sortBy (\(_, a) (_, b) -> compare b a) kvs

main :: IO ()
main = do
  print (pairsWith [3, 1, 4, 1, 5 :: Int])
  putStrLn (sizeOfEmpty "abc")
  print (largest [("a", 3), ("b", 7), ("c", 5 :: Int)])
