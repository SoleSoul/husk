-- System.Directory: files and directories.  Errors are IOErrors, shown
-- like GHC's ("todo.txt: removeFile: does not exist (...)").
module System.Directory (
  doesFileExist, doesDirectoryExist, doesPathExist,
  removeFile, renameFile, copyFile, getFileSize,
  createDirectory, createDirectoryIfMissing, removeDirectory, removeDirectoryRecursive,
  renameDirectory, listDirectory, getDirectoryContents,
  getCurrentDirectory, setCurrentDirectory, withCurrentDirectory,
  getHomeDirectory, getTemporaryDirectory, makeAbsolute
  ) where
import Husk.Base

foreign import prim "hs-does-file-exist" doesFileExist :: FilePath -> IO Bool
foreign import prim "hs-does-directory-exist" doesDirectoryExist :: FilePath -> IO Bool
foreign import prim "hs-remove-file" removeFile :: FilePath -> IO ()
foreign import prim "hs-rename-file" renameFile :: FilePath -> FilePath -> IO ()
foreign import prim "hs-create-directory" createDirectory :: FilePath -> IO ()
foreign import prim "hs-remove-directory" removeDirectory :: FilePath -> IO ()
foreign import prim "hs-list-directory" listDirectory :: FilePath -> IO [FilePath]
foreign import prim "hs-get-current-directory" getCurrentDirectory :: IO FilePath
foreign import prim "hs-set-current-directory" setCurrentDirectory :: FilePath -> IO ()
foreign import prim "hs-get-file-size" getFileSize :: FilePath -> IO Integer
foreign import prim "hs-read-file-bytes" primReadFileBytes :: FilePath -> IO String
foreign import prim "hs-write-file-bytes" primWriteFileBytes :: FilePath -> String -> IO ()

doesPathExist :: FilePath -> IO Bool
doesPathExist p = (||) <$> doesFileExist p <*> doesDirectoryExist p

renameDirectory :: FilePath -> FilePath -> IO ()
renameDirectory = renameFile

copyFile :: FilePath -> FilePath -> IO ()
copyFile from to = primReadFileBytes from >>= primWriteFileBytes to

-- like listDirectory, but including "." and ".."
getDirectoryContents :: FilePath -> IO [FilePath]
getDirectoryContents p = fmap ([".", ".."] ++) (listDirectory p)

-- createDirectoryIfMissing True also creates missing parent directories
createDirectoryIfMissing :: Bool -> FilePath -> IO ()
createDirectoryIfMissing parents path = mapM_ create (if parents then prefixes else [path])
  where
    create p = do
      exists <- doesDirectoryExist p
      if exists then return () else createDirectory p
    -- "a/b/c" -> ["a", "a/b", "a/b/c"]
    prefixes = [take n path | n <- [1 .. length path], n == length path || path !! n == '/']

removeDirectoryRecursive :: FilePath -> IO ()
removeDirectoryRecursive dir = do
  entries <- listDirectory dir
  mapM_ remove [dir ++ "/" ++ e | e <- entries]
  removeDirectory dir
  where
    remove p = do
      isDir <- doesDirectoryExist p
      if isDir then removeDirectoryRecursive p else removeFile p

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory dir action = do
  old <- getCurrentDirectory
  setCurrentDirectory dir
  r <- action `catch` (\e -> setCurrentDirectory old >> throwIO e)
  setCurrentDirectory old
  return r

getHomeDirectory :: IO FilePath
getHomeDirectory = fmap (maybe "/" id) (lookupEnv "HOME")

getTemporaryDirectory :: IO FilePath
getTemporaryDirectory = fmap (maybe "/tmp" id) (lookupEnv "TMPDIR")

makeAbsolute :: FilePath -> IO FilePath
makeAbsolute p@('/' : _) = return p
makeAbsolute p = do
  cwd <- getCurrentDirectory
  return (if p == "." then cwd else cwd ++ "/" ++ p)
