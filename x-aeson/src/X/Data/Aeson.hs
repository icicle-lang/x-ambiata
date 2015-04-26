module X.Data.Aeson (
    (.=?)
  , asText
  , as
  ) where

import           Data.Aeson
import           Data.Text

import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Data.ByteString.Lazy (toStrict)


(.=?) :: ToJSON a => Text -> Maybe a -> [(Text, Value)]
(.=?) k =
  maybe [] (\x -> [k .= x])

asText :: ToJSON a => a -> Text
asText =
  decodeUtf8 . toStrict . encode

as :: FromJSON a => Text -> Either Text a
as =
  either (Left . pack) Right . eitherDecodeStrict . encodeUtf8
