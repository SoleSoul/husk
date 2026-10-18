-- Data.IntSet: sets of Ints (implemented as Data.Set Int)
module Data.IntSet (IntSet, Key, module Data.Set) where
import Data.Set
import qualified Data.Set.Internal as S

type IntSet = S.Set Int
type Key = Int
