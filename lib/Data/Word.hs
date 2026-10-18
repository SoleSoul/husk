-- Data.Word: unsigned integer types.  In husk these are synonyms for Int
-- (no wrap-around on overflow, and negative values are not rejected).
module Data.Word (Word, Word8, Word16, Word32, Word64) where
import Husk.Base

type Word8 = Int
type Word16 = Int
type Word32 = Int
type Word64 = Int
