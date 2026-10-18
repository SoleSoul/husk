-- Control.Monad.Writer: the writer monad (mtl style)
module Control.Monad.Writer (
  module Control.Monad.Trans.Writer, module Control.Monad, MonadTrans(..), MonadIO(..)
  ) where

import Control.Monad
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
