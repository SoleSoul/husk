module Control.Exception (
  SomeException, catch, try, handle, evaluate, throwIO, throw, bracket, finally,
  displayException, ErrorCall
  ) where
import Husk.Base

type ErrorCall = SomeException
