-- System.IO.Error: IO errors.  husk has a single exception type, so an
-- IOError is a SomeException; these functions recognize IO errors by their
-- messages, which follow GHC's ("todo.txt: openFile: does not exist (...)").
module System.IO.Error (
  IOError, ioError, userError, catchIOError, tryIOError, isIOError,
  isDoesNotExistError, isAlreadyExistsError, isPermissionError, isAlreadyInUseError,
  isEOFError, isUserError, isIllegalOperation,
  ioeGetErrorString, ioeGetFileName
  ) where
import Husk.Base

-- catches IO errors only; other exceptions (error, ...) pass through
catchIOError :: IO a -> (IOError -> IO a) -> IO a
catchIOError a h = catch a (\e -> if isIOError e then h e else throwIO e)

tryIOError :: IO a -> IO (Either IOError a)
tryIOError a = catchIOError (fmap Right a) (return . Left)

isIOError :: SomeException -> Bool
isIOError e = isUserError e || isEOFError e || isJust (fileError e)

-- "path: who: kind (os message)" -> (path, kind)
fileError :: SomeException -> Maybe (String, String)
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

hasKind :: String -> SomeException -> Bool
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
