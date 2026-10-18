-- Control.Monad.State: the state monad (mtl style).
--   State s a  is  StateT s Identity a;  use lift / liftIO to reach the
--   monad underneath a StateT.
module Control.Monad.State (
  module Control.Monad.Trans.State, module Control.Monad, MonadTrans(..), MonadIO(..)
  ) where

import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
