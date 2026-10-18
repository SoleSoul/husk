newtype Size = Size Int
instance Monoid Size where
  mempty = Size 0
  mappend (Size a) (Size b) = Size (a + b)
main = return ()
