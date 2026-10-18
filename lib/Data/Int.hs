-- Data.Int: sized integer types.  In husk these are all synonyms for Int
-- (arbitrary precision, no wrap-around on overflow).
module Data.Int (Int, Int8, Int16, Int32, Int64) where
import Husk.Base

type Int8 = Int
type Int16 = Int
type Int32 = Int
type Int64 = Int
