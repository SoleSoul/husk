-- Data.IntMap: maps with Int keys (implemented as Data.Map Int)
module Data.IntMap (IntMap, Key, module Data.Map) where
import Data.Map
import qualified Data.Map.Internal as M

type IntMap = M.Map Int
type Key = Int
