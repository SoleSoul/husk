{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Show, Functor, Foldable, Traversable)
data Rose a = Rose a [Rose a] deriving (Show, Functor, Foldable, Traversable)
data Pair b a = Pair b a a deriving (Show, Functor, Foldable, Traversable)


build :: [a] -> Tree a
build [] = Leaf
build xs = let (l, x:r) = splitAt (length xs `div` 2) xs in Node (build l) x (build r)

main = do
  let t = build [1 .. 7 :: Int]
  print (sum t, product t, length t, maximum t, foldr (:) [] t, elem 4 t)
  print (traverse (\x -> if x > 0 then Just (x * 10) else Nothing) t)
  print (fmap show (Rose 1 [Rose 2 [], Rose 3 [Rose 4 []]]))
  print (sum (Rose 1 [Rose 2 [], Rose 3 [Rose 4 []]]), concatMap show (Pair 'x' 1 2))
  mapM_ print (Pair "label" 'a' 'b')
  print (traverse (\x -> [x, x + 1]) (Pair (0 :: Int) 1 (2 :: Int)))
