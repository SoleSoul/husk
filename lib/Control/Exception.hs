module Control.Exception (
  SomeException, catch, try, handle, evaluate, throwIO, throw, bracket, bracket_, bracketOnError,
  finally, onException,
  displayException, ErrorCall
  ) where
import Husk.Base

type ErrorCall = SomeException
