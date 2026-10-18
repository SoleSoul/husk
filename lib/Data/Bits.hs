module Data.Bits (Bits(..), (.<<.), (.>>.)) where
import Husk.Base

(.<<.) :: Bits a => a -> Int -> a
(.<<.) = shiftL
(.>>.) :: Bits a => a -> Int -> a
(.>>.) = shiftR
infixl 8 .<<., .>>.
