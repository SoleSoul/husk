-- Data.Array.IO: mutable arrays in the IO monad (see Data.Array.Base)
module Data.Array.IO (
  IOArray, IOUArray, MArrayMonad, newArray, newArray_, newListArray, readArray, writeArray,
  modifyArray, modifyArray', getBounds, getElems, getAssocs, freeze, thaw,
  unsafeFreeze, unsafeThaw, module Data.Array
  ) where
import Data.Array
import Data.Array.Base
