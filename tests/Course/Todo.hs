-- like the Helsinki Haskell MOOC's Mooc.Todo: a placeholder that throws
module Course.Todo where

import Control.Exception (Exception, throw)

data TODO = TODO deriving Show
instance Exception TODO

todo :: a
todo = throw TODO
