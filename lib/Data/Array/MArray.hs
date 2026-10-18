-- Data.Array.MArray: the mutable array operations (see Data.Array.Base)
module Data.Array.MArray (
  MArrayMonad, newArray, newArray_, newListArray, readArray, writeArray,
  modifyArray, modifyArray', getBounds, getElems, getAssocs, freeze, thaw,
  unsafeFreeze, unsafeThaw, module Data.Array
  ) where
import Data.Array
import Data.Array.Base
