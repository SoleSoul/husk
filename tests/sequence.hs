import qualified Data.Sequence as Seq
import Data.Sequence (Seq(..), (|>), (<|), (><), ViewL(..), viewl)
import Data.Foldable (toList)

-- breadth-first numbering of a tree using a Seq as a queue
data Tree = Node String [Tree]

bfsOrder :: Tree -> [String]
bfsOrder t = go (Seq.singleton t)
  where
    go Empty = []
    go (Node name kids :<| rest) = name : go (foldl (|>) rest kids)

lastTwo :: Seq a -> Maybe (a, a)
lastTwo (_ :|> a :|> b) = Just (a, b)
lastTwo _ = Nothing

main :: IO ()
main = do
  let s = Seq.fromList [1 .. 10 :: Int]
  print s
  print (Seq.length s, Seq.index s 3, Seq.lookup 20 s, toList (Seq.reverse (Seq.take 3 s)))
  print ((0 <| Seq.fromList [1, 2]) |> 3, Seq.fromList "ab" >< Seq.fromList "cd")
  print (Seq.update 0 100 s, Seq.adjust (* 2) 9 s)
  print (Seq.splitAt 4 s, Seq.insertAt 2 99 (Seq.fromList [1, 2, 3]), Seq.deleteAt 0 (Seq.fromList "xyz"))
  print (sum s, maximum s, fmap show (Seq.fromList [1, 2 :: Int]))
  print (bfsOrder (Node "a" [Node "b" [Node "d" []], Node "c" [Node "e" [], Node "f" []]]))
  print (lastTwo (Seq.fromList "hello"), lastTwo (Seq.fromList "x"))
  case viewl (Seq.fromList [5, 6]) of
    x :< rest -> print (x, rest)
    EmptyL -> putStrLn "empty"
  let big = foldl (|>) Seq.empty [1 .. 100000 :: Int]
  print (Seq.length big, Seq.index big 50000, sum (Seq.drop 99990 big))
  print (Seq.sort (Seq.fromList [3, 1, 2 :: Int]), Empty :: Seq Int, 1 :<| (2 :<| Empty))
