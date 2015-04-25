{-# LANGUAGE OverloadedStrings #-}
module X.Data.Aeson (
    (.=?)
  , asText
  , as
  , parsePair
  , printPair
  ) where

import           Control.Applicative

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Monoid
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

parsePair :: Text -> Text -> Value -> Parser (Text, Text)
parsePair k v (Object o) =
  (,) <$> o .: k <*> o .: v
parsePair k v _ =
  fail . unpack $ "Invalid pair, expected object with the following keys: " <> intercalate ", " [k, v]

printPair :: Text -> Text -> (Text, Text) -> Value
printPair k v (k', v') =
  object [ k .= k', v  .= v' ]
