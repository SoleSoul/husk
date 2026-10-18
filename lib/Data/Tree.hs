-- Data.Tree: rose trees (multi-way trees), as in the containers package.
module Data.Tree (
  Tree(..), Forest, flatten, levels, foldTree, unfoldTree, unfoldForest,
  drawTree, drawForest
  ) where
import Husk.Base

data Tree a = Node { rootLabel :: a, subForest :: [Tree a] }
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

type Forest a = [Tree a]

instance Applicative Tree where
  pure x = Node x []
  Node f tfs <*> tx@(Node x txs) = Node (f x) (map (f <$>) txs ++ map (<*> tx) tfs)

instance Monad Tree where
  Node x ts >>= f = case f x of
    Node x' ts' -> Node x' (ts' ++ map (>>= f) ts)

flatten :: Tree a -> [a]
flatten t = go t []
  where go (Node x ts) xs = x : foldr go xs ts

levels :: Tree a -> [[a]]
levels t = map (map rootLabel) (takeWhile (not . null) (iterate (concatMap subForest) [t]))

foldTree :: (a -> [b] -> b) -> Tree a -> b
foldTree f = go where go (Node x ts) = f x (map go ts)

unfoldTree :: (b -> (a, [b])) -> b -> Tree a
unfoldTree f b = let (a, bs) = f b in Node a (unfoldForest f bs)

unfoldForest :: (b -> (a, [b])) -> [b] -> [Tree a]
unfoldForest f = map (unfoldTree f)

drawTree :: Tree String -> String
drawTree = unlines . draw

drawForest :: [Tree String] -> String
drawForest = unlines . map drawTree

draw :: Tree String -> [String]
draw (Node x ts0) = lines x ++ drawSubTrees ts0
  where
    drawSubTrees [] = []
    drawSubTrees [t] = "|" : shift "`- " "   " (draw t)
    drawSubTrees (t : ts) = "|" : shift "+- " "|  " (draw t) ++ drawSubTrees ts
    shift first other = zipWith (++) (first : repeat other)
