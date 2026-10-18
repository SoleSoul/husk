-- a file without a header or main, loaded with :l (as in GHCi)
double :: Int -> Int
double x = x * 2

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)
