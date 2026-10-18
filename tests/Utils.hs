module Utils where

import Geometry.Shapes

totalArea :: [Shape] -> Double
totalArea = sum . map area

class Pretty a where
  pretty :: a -> String

instance Pretty Shape where
  pretty (Circle r) = "circle of radius " ++ show r
  pretty (Square s) = "square of side " ++ show s
