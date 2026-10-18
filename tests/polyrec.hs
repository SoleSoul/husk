data Nested a = Flat a | Nest (Nested [a])

showNested :: Show a => Nested a -> String
showNested (Flat x) = show x
showNested (Nest n) = "Nest(" ++ showNested n ++ ")"

main = putStrLn (showNested (Nest (Nest (Flat [[1 :: Int, 2]]))))
