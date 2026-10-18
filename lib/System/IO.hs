module System.IO (
  IO, FilePath, Handle, stdin, stdout, stderr,
  putChar, putStr, putStrLn, print, getChar, getLine, getContents, interact, isEOF,
  readFile, writeFile, appendFile, readIO, readLn,
  hPutStr, hPutStrLn, hPrint, hGetLine, hGetContents, hFlush, hClose, hIsEOF,
  BufferMode(..), hSetBuffering, hGetBuffering, hSetEcho,
  IOMode(..), openFile, withFile
  ) where
import Husk.Base
