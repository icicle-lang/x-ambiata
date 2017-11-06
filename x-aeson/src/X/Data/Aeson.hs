{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module X.Data.Aeson (
    module X
  , (.=?)
  , asText
  , as
  , asTextWith
  , asWith
  , parsePair
  , printPair
  , valueToObject
  , valueToList
  , valueFromList
  , valueToObjectList
  , objectFromList
  , parseMaybeFailO
  , parseMaybeFail
  , parseEitherFailO
  , parseEitherFail
  , mapToJson
  , mapFromJson
  ) where

import           Control.Applicative ((<*>), pure)
import           Control.Monad ((>>=), (=<<), fail)
import           Data.Aeson as X
import           Data.Aeson.Types as X
import           Data.Either (Either(..), either)
import           Data.Function ((.), ($))
import           Data.Functor ((<$>), fmap)
import           Data.Map (Map)
import qualified Data.Map as M
import qualified Data.HashMap.Strict as H
import           Data.Maybe (Maybe (..), maybe)
import           Data.Monoid
import           Data.Ord (Ord)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Data.Traversable (for, traverse)
import qualified Data.Vector as V
import           Data.ByteString.Lazy (toStrict)


(.=?) :: ToJSON a => Text -> Maybe a -> [(Text, Value)]
(.=?) k =
  maybe [] (\x -> [k .= x])

asText :: ToJSON a => a -> Text
asText =
  decodeUtf8 . toStrict . encode

as :: FromJSON a => Text -> Either Text a
as =
  either (Left . T.pack) Right . eitherDecodeStrict . encodeUtf8

asTextWith :: (a -> Value) -> a -> Text
asTextWith from =
  asText . from

asWith :: (Value -> Parser a) -> Text -> Either Text a
asWith to t =
  as t >>= \a' -> case parse to a' of
    Success a -> pure a
    Error msg -> Left . T.pack $ msg

parsePair :: Text -> Text -> Value -> Parser (Text, Text)
parsePair k v (Object o) =
  (,) <$> o .: k <*> o .: v
parsePair k v _ =
  fail . T.unpack $ "Invalid pair, expected object with the following keys: " <> T.intercalate ", " [k, v]

printPair :: Text -> Text -> (Text, Text) -> Value
printPair k v (k', v') =
  object [ k .= k', v  .= v' ]

valueToObject :: Text -> Value -> Parser Object
valueToObject t = \case
  Object o -> pure o
  v -> typeMismatch (T.unpack t) v

valueToList :: Text -> Value -> Parser [Value]
valueToList t = \case
  Array a -> pure $ V.toList a
  v -> typeMismatch (T.unpack t) v

valueFromList :: [Value] -> Value
valueFromList =
  Array . V.fromList

valueToObjectList :: Text -> Value -> Parser [Object]
valueToObjectList t =
  (=<<) (traverse (valueToObject t)) . valueToList t

objectFromList :: [(Text, Value)] -> Object
objectFromList =
  H.fromList

parseEitherFailO :: FromJSON a => (b -> Text) -> (a -> Either b c) -> Text -> Object -> Parser c
parseEitherFailO e f p o =
  parseEitherFail e f =<< o .: p

parseEitherFail :: (b -> Text) -> (a -> Either b c) -> a -> Parser c
parseEitherFail e f =
  either (fail . T.unpack . e) pure . f

parseMaybeFailO :: FromJSON a => Text -> (a -> Maybe b) -> Text -> Object -> Parser b
parseMaybeFailO e f p o =
  parseMaybeFail e f =<< o .: p

parseMaybeFail :: Text -> (a -> Maybe b) -> a -> Parser b
parseMaybeFail e f =
  maybe (fail $ T.unpack e) pure . f

mapToJson :: (k -> Text) -> (v -> Value) -> Map k v -> Object
mapToJson k v =
   objectFromList . fmap (\(a,b) -> (k a, v b)) . M.toList

mapFromJson :: Ord k => (Text -> Parser k) -> (Value -> Parser v) -> Object -> Parser (Map k v)
mapFromJson k v o =
  fmap M.fromList . for (H.toList o) $ \(k', v') ->
    (,)
      <$> k k'
      <*> v v'
