-- Data.Array.Base: mutable arrays, shared by Data.Array.IO, Data.Array.ST and
-- Data.Array.MArray.  GHC overloads these operations with the multi-parameter
-- class MArray; husk uses one array type and a class over the monad instead,
-- so the same functions work in IO and in ST.
module Data.Array.Base where

import Husk.Base
import Data.Array
import Control.Monad.ST

data MVector a

foreign import lazy "hs-new-marray" primNewMVector :: Int -> a -> IO (MVector a)
foreign import prim "hs-marray-read" primReadMVector :: MVector a -> Int -> IO a
foreign import lazy "hs-marray-write" primWriteMVector :: MVector a -> Int -> a -> IO ()
foreign import prim "hs-marray-freeze" primFreezeMVector :: MVector a -> IO (Vector a)
foreign import prim "hs-marray-thaw" primThawVector :: Vector a -> IO (MVector a)
foreign import prim "hs-unsafe-coerce" primUnsafeFreeze :: MVector a -> Vector a
foreign import prim "hs-unsafe-coerce" primUnsafeThaw :: Vector a -> MVector a

-- | A mutable array (used for IOArray, IOUArray, STArray and STUArray)
data IOArray i e = IOArray (i, i) Int (MVector e)
type IOUArray = IOArray
type STArray s = IOArray
type STUArray s = IOArray

-- | Monads in which mutable arrays can be used
class Monad m => MArrayMonad m where
  liftArray :: IO a -> m a

instance MArrayMonad IO where
  liftArray io = io

instance MArrayMonad (ST s) where
  liftArray io = primUnsafeCoerce io

newArray :: (Ix i, MArrayMonad m) => (i, i) -> e -> m (IOArray i e)
newArray b x = liftArray (let n = rangeSize b in primNewMVector n x >>= \v -> return (IOArray b n v))

newArray_ :: (Ix i, MArrayMonad m) => (i, i) -> m (IOArray i e)
newArray_ b = newArray b (error "undefined array element")

newListArray :: (Ix i, MArrayMonad m) => (i, i) -> [e] -> m (IOArray i e)
newListArray b es = thaw (listArray b es)

readArray :: (Ix i, MArrayMonad m) => IOArray i e -> i -> m e
readArray (IOArray b _ v) i = liftArray (primReadMVector v (index b i))

writeArray :: (Ix i, MArrayMonad m) => IOArray i e -> i -> e -> m ()
writeArray (IOArray b _ v) i x = liftArray (primWriteMVector v (index b i) x)

modifyArray :: (Ix i, MArrayMonad m) => IOArray i e -> i -> (e -> e) -> m ()
modifyArray arr i f = readArray arr i >>= \x -> writeArray arr i (f x)

modifyArray' :: (Ix i, MArrayMonad m) => IOArray i e -> i -> (e -> e) -> m ()
modifyArray' arr i f = readArray arr i >>= \x -> let y = f x in y `seq` writeArray arr i y

getBounds :: (Ix i, MArrayMonad m) => IOArray i e -> m (i, i)
getBounds (IOArray b _ _) = return b

getElems :: (Ix i, MArrayMonad m) => IOArray i e -> m [e]
getElems arr = fmap elems (freeze arr)

getAssocs :: (Ix i, MArrayMonad m) => IOArray i e -> m [(i, e)]
getAssocs arr = fmap assocs (freeze arr)

freeze :: (Ix i, MArrayMonad m) => IOArray i e -> m (Array i e)
freeze (IOArray b n v) = liftArray (primFreezeMVector v >>= \vec -> return (MkArray b n vec))

unsafeFreeze :: (Ix i, MArrayMonad m) => IOArray i e -> m (Array i e)
unsafeFreeze (IOArray b n v) = return (MkArray b n (primUnsafeFreeze v))

thaw :: (Ix i, MArrayMonad m) => Array i e -> m (IOArray i e)
thaw (MkArray b n vec) = liftArray (primThawVector vec >>= \v -> return (IOArray b n v))

unsafeThaw :: (Ix i, MArrayMonad m) => Array i e -> m (IOArray i e)
unsafeThaw (MkArray b n vec) = return (IOArray b n (primUnsafeThaw vec))

runSTArray :: Ix i => ST s (IOArray i e) -> Array i e
runSTArray st = runST (st >>= unsafeFreeze)

runSTUArray :: Ix i => ST s (IOArray i e) -> Array i e
runSTUArray = runSTArray
