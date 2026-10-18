-- Data.IntMap.Strict: maps with Int keys (implemented as Data.Map.Strict Int)
module Data.IntMap.Strict (IntMap, Key, module Data.Map.Strict) where
import Data.Map.Strict
import qualified Data.Map.Internal as M

type IntMap = M.Map Int
type Key = Int
