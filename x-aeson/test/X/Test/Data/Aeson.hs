{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module X.Test.Data.Aeson where

import           Control.Applicative

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text as T

import           Disorder.Aeson

import           System.IO ()

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           X.Data.Aeson


data Foo = Foo { foo :: Maybe Text } deriving (Eq, Show)

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
prop_maybe = jsonProp

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
  asWith (parseJSON :: Value -> Parser Text) t === as t

return []
tests :: IO Bool
tests = $quickCheckAll
