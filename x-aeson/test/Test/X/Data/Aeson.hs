{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.X.Data.Aeson where

import           Control.Monad
import           Data.Bool
import           Data.Either
import           Data.Eq
import           Data.Function
import           Data.Functor ((<$>))
import           Data.Maybe
import           Data.Text as T
import           System.IO (IO)
import           Text.Show

import           Disorder.Aeson
import           Disorder.Core

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           X.Data.Aeson


newtype Foo = Foo {
    foo :: Maybe Text
  } deriving (Eq, Show)

instance Arbitrary Foo where
  arbitrary =
    Foo <$> arbitrary

instance ToJSON Foo where
 toJSON (Foo f) =
   object $ "foo" .=? f

instance FromJSON Foo where
  parseJSON (Object o) =
    Foo <$> o .:? "foo"
  parseJSON _ =
    fail "Invalid JSON for Foo"

prop_maybe :: Foo -> Property
prop_maybe =
  jsonProp

prop_text :: Foo -> Property
prop_text f =
  as (asText f) === Right f

prop_pair :: Text -> Text -> (Text, Text) -> Property
prop_pair k v p =
  (k /= v) ==>
    parseMaybe (const $ parsePair k v (printPair k v p)) () === Just p

prop_asText :: Text -> Property
prop_asText t =
  asTextWith (toJSON :: Text -> Value) t === asText t

prop_as :: Text -> Property
prop_as t =
  discardError (asWith (parseJSON :: Value -> Parser Text) t)
  ===
  discardError (as t)

discardError :: Either a b -> Either () b
discardError = \case
  Left _ ->
    Left ()
  Right x ->
    Right x

return []
tests :: IO Bool
tests =
  $disorderCheckEnvAll TestRunMore
