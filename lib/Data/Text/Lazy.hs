-- Data.Text.Lazy: lazy Text.  In husk this is the same type as strict Text
-- (which is lazy already); this module adds the conversions.
module Data.Text.Lazy (module Data.Text, fromStrict, toStrict, fromChunks, toChunks) where
import Prelude ()
import Data.Text
import qualified Data.Text as T

fromStrict, toStrict :: Text -> Text
fromStrict t = t
toStrict t = t

fromChunks :: [Text] -> Text
fromChunks = T.concat

toChunks :: Text -> [Text]
toChunks t = [t]
