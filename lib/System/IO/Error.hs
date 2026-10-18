-- System.IO.Error: IO errors.  The predicates recognize the kind of error
-- by its message, which follows GHC's ("todo.txt: openFile: does not exist (...)").
module System.IO.Error (
  IOError, ioError, userError, catchIOError, tryIOError,
  isDoesNotExistError, isAlreadyExistsError, isPermissionError, isAlreadyInUseError,
  isEOFError, isUserError, isIllegalOperation,
  ioeGetErrorString, ioeGetFileName
  ) where
import Husk.Base

-- catches IO errors only; other exceptions (error, ...) pass through
catchIOError :: IO a -> (IOError -> IO a) -> IO a
catchIOError = catch

tryIOError :: IO a -> IO (Either IOError a)
tryIOError a = catchIOError (fmap Right a) (return . Left)

-- "path: who: kind (os message)" -> (path, kind)
fileError :: IOError -> Maybe (String, String)
fileError e = case splitOn' (show e) of
  (path : _ : rest : _) | any (`isPrefixOf` rest) kinds -> Just (path, takeWhile (/= '(') rest)
  _ -> Nothing
  where
    kinds = ["does not exist", "already exists", "permission denied", "resource busy",
             "inappropriate type", "unsatisfied constraints", "failed"]
    splitOn' s = case breakSep s of
      (a, Nothing) -> [a]
      (a, Just rest) -> a : splitOn' rest
    breakSep [] = ([], Nothing)
    breakSep (':' : ' ' : rest) = ([], Just rest)
    breakSep (c : rest) = let (a, r) = breakSep rest in (c : a, r)

hasKind :: String -> IOError -> Bool
hasKind k e = maybe False ((k `isPrefixOf`) . snd) (fileError e)

isDoesNotExistError, isAlreadyExistsError, isPermissionError, isAlreadyInUseError,
  isEOFError, isUserError, isIllegalOperation :: IOError -> Bool
isDoesNotExistError = hasKind "does not exist"
isAlreadyExistsError = hasKind "already exists"
isPermissionError = hasKind "permission denied"
isAlreadyInUseError = hasKind "resource busy"
isIllegalOperation = hasKind "inappropriate type"
isEOFError e = "end of file" `isSuffixOf` show e
isUserError e = "user error (" `isPrefixOf` show e

ioeGetFileName :: IOError -> Maybe FilePath
ioeGetFileName e = fmap fst (fileError e)

-- the message of a user error, or the kind of other IO errors
ioeGetErrorString :: IOError -> String
ioeGetErrorString e
  | isUserError e = init (drop (length "user error (") (show e))
  | otherwise = maybe (show e) (dropWhileEnd (== ' ') . snd) (fileError e)
