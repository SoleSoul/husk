import qualified Data.Map as M
import qualified Data.Map.Strict as MS
import qualified Data.Set as S
import qualified Data.IntMap as IM
import Data.List (foldl')

wordFreq :: String -> M.Map String Int
wordFreq = foldl' (\m w -> MS.insertWith (+) w 1 m) M.empty . words

main :: IO ()
main = do
  let m = M.fromList [(3, "c"), (1, "a"), (2, "b")]
  print m
  print (M.lookup 2 m, M.lookup 5 m, M.member 1 m, M.size m)
  print (M.insert 4 "d" m)
  print (M.delete 2 m, M.adjust (++ "!") 1 m)
  print (M.toList (M.map length m), M.keys m, M.elems m)
  print (M.findWithDefault "?" 9 m, m M.! 3)
  print (wordFreq "a b a c b a")
  print (M.toList (M.unionWith (+) (M.fromList [(1, 1), (2, 2)]) (M.fromList [(2, 10), (3, 30)])))
  print (M.filter even (M.fromList (zip "abcdef" [1 ..])))
  print (M.foldrWithKey (\k v acc -> k + v + acc) 0 (M.fromList [(1, 10), (2, 20)]))
  print (M.lookupMin m, M.lookupMax m, M.findMin m)
  print (M.alter (fmap (++ "?")) 1 m, M.alter (const (Just "new")) 7 m)
  print (M.lookupLT 2 m, M.lookupGE 4 m, M.lookupLE 2 m)
  print (sum (M.fromList [(1, 5), (2, 6)]), length m, fmap reverse (M.fromList [(1, "ab")]))
  let big = M.fromList [(i, i * i) | i <- [1 .. 10000 :: Int]]
  print (M.size big, M.lookup 9999 big, M.foldr (+) 0 big)
  print (M.size (foldr M.delete big [1 .. 5000]))
  print (M.intersectionWith (,) (M.fromList [(1, 'a'), (2, 'b')]) (M.fromList [(2, "x"), (3, "y")]))
  print (M.difference (M.fromList [(1, 'a'), (2, 'b')]) (M.fromList [(2, ())]))
  let s = S.fromList "mississippi"
  print s
  print (S.member 's' s, S.size s, S.toList (S.map succ s))
  print (S.union (S.fromList [1, 2, 3]) (S.fromList [3, 4 :: Int]), S.intersection (S.fromList [1, 2, 3]) (S.fromList [2, 3, 4 :: Int]))
  print (S.difference (S.fromList [1 .. 10]) (S.fromList [2, 4 .. 10 :: Int]))
  print (S.size (S.fromList [x `mod` 1000 | x <- [1 .. 100000 :: Int]]))
  print (IM.toList (IM.fromListWith (++) [(1, "a"), (2, "b"), (1, "c")]))
  print (S.elems (S.insert 0 (S.fromList [5, 3 :: Int])), S.findMin (S.fromList "hello"))
