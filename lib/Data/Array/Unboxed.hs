-- Data.Array.Unboxed: in husk, UArray is the same as Array
module Data.Array.Unboxed (UArray, module Data.Array) where
import Data.Array
type UArray = Array
