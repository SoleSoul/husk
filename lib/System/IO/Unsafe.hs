-- System.IO.Unsafe: running IO actions outside IO.
module System.IO.Unsafe (unsafePerformIO, unsafeInterleaveIO) where
import Husk.Base

-- (husk runs the action when the result is demanded)
unsafeInterleaveIO :: IO a -> IO a
unsafeInterleaveIO io = return (unsafePerformIO io)
