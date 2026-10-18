-- Control.Monad.Reader: the reader monad (mtl style)
module Control.Monad.Reader (
  module Control.Monad.Trans.Reader, module Control.Monad, MonadTrans(..), MonadIO(..)
  ) where

import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
