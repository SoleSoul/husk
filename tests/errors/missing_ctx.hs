showAll :: [a] -> String
showAll xs = concatMap show xs
main = putStrLn (showAll [1,2])
