module Geometry.Shapes (Shape(..), area, unitSquare) where

data Shape = Circle Double | Square Double deriving (Show, Eq)

area :: Shape -> Double
area (Circle r) = 3 * r * r
area (Square s) = s * s

unitSquare :: Shape
unitSquare = Square 1

secret :: Int
secret = 42
