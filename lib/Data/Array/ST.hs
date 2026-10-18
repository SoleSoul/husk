-- Data.Array.ST: mutable arrays in the ST monad (see Data.Array.Base)
module Data.Array.ST (
  STArray, STUArray, MArrayMonad, newArray, newArray_, newListArray, readArray, writeArray,
  modifyArray, modifyArray', getBounds, getElems, getAssocs, freeze, thaw,
  unsafeFreeze, unsafeThaw, runSTArray, runSTUArray, module Data.Array, module Control.Monad.ST
  ) where
import Data.Array
import Control.Monad.ST
import Data.Array.Base
