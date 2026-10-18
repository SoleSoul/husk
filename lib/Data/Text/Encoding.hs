-- Data.Text.Encoding: converting Text to and from UTF-8 ByteStrings.
module Data.Text.Encoding (
  encodeUtf8, decodeUtf8, decodeUtf8', decodeLatin1, UnicodeException
  ) where
import Husk.Base
import qualified Husk.ByteString as B
import Data.Text (Text, pack, unpack)

-- the error of decodeUtf8' (in the text package it is in Data.Text.Encoding.Error)
data UnicodeException = DecodeError String

instance Show UnicodeException where
  show (DecodeError msg) = msg

instance Exception UnicodeException

encodeUtf8 :: Text -> B.ByteString
encodeUtf8 t = B.BS (B.encodeUtf8 (unpack t))

-- invalid UTF-8 is an error
decodeUtf8 :: B.ByteString -> Text
decodeUtf8 b = either throw id (decodeUtf8' b)

decodeUtf8' :: B.ByteString -> Either UnicodeException Text
decodeUtf8' (B.BS s) = case B.decodeUtf8Strict s of
  Just t -> Right (pack t)
  Nothing -> Left (DecodeError "Data.Text.Encoding: Invalid UTF-8 stream")

decodeLatin1 :: B.ByteString -> Text
decodeLatin1 (B.BS s) = pack s
