-- Data.STRef: mutable references in the ST monad
module Data.STRef (STRef, newSTRef, readSTRef, writeSTRef, modifySTRef, modifySTRef') where

import Husk.Base
import Control.Monad.ST

newtype STRef s a = STRef (IORef a)

newSTRef :: a -> ST s (STRef s a)
newSTRef x = primUnsafeCoerce (fmap STRef (newIORef x))

readSTRef :: STRef s a -> ST s a
readSTRef (STRef r) = primUnsafeCoerce (readIORef r)

writeSTRef :: STRef s a -> a -> ST s ()
writeSTRef (STRef r) x = primUnsafeCoerce (writeIORef r x)

modifySTRef :: STRef s a -> (a -> a) -> ST s ()
modifySTRef (STRef r) f = primUnsafeCoerce (modifyIORef r f)

modifySTRef' :: STRef s a -> (a -> a) -> ST s ()
modifySTRef' (STRef r) f = primUnsafeCoerce (modifyIORef' r f)
