-- Data.Text.IO: Text versions of the basic IO functions
module Data.Text.IO (putStr, putStrLn, getLine, getContents, interact, readFile, writeFile,
                     appendFile, hPutStr, hPutStrLn, hGetLine, hGetContents) where

import Prelude hiding (putStr, putStrLn, getLine, getContents, interact, readFile, writeFile, appendFile)
import qualified Prelude as P
import qualified System.IO as IO
import Data.Text (Text, pack, unpack)

putStr :: Text -> IO ()
putStr = P.putStr . unpack

putStrLn :: Text -> IO ()
putStrLn = P.putStrLn . unpack

getLine :: IO Text
getLine = fmap pack P.getLine

getContents :: IO Text
getContents = fmap pack P.getContents

interact :: (Text -> Text) -> IO ()
interact f = P.interact (unpack . f . pack)

readFile :: FilePath -> IO Text
readFile p = fmap pack (P.readFile p)

writeFile :: FilePath -> Text -> IO ()
writeFile p = P.writeFile p . unpack

appendFile :: FilePath -> Text -> IO ()
appendFile p = P.appendFile p . unpack

hPutStr :: IO.Handle -> Text -> IO ()
hPutStr h = IO.hPutStr h . unpack

hPutStrLn :: IO.Handle -> Text -> IO ()
hPutStrLn h = IO.hPutStrLn h . unpack

hGetLine :: IO.Handle -> IO Text
hGetLine h = fmap pack (IO.hGetLine h)

hGetContents :: IO.Handle -> IO Text
hGetContents h = fmap pack (IO.hGetContents h)
