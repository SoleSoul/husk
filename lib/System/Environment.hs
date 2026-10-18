module System.Environment (getArgs, getProgName, lookupEnv, getEnv) where
import Husk.Base

getEnv :: String -> IO String
getEnv name = lookupEnv name >>= \m -> case m of
  Just v -> return v
  Nothing -> ioError (userError (name ++ ": getEnv: does not exist"))
