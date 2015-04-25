{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module X.Test.Data.Aeson where

import           Control.Applicative

import           Data.Aeson
import           Data.Text

import           Orphanarium.Aeson

import           System.IO ()

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           X.Data.Aeson


data Foo = Foo { foo :: Maybe Text } deriving (Eq, Show)

instance Arbitrary Foo where
  arbitrary = Foo <$> arbitrary

instance ToJSON Foo where
 toJSON (Foo f) =
   object $ "foo" .=? f

instance FromJSON Foo where
  parseJSON (Object o) = Foo
    <$> o .:? "foo"
  parseJSON _ = fail "Invalid JSON for Foo"

prop_maybe :: Foo -> Property
prop_maybe = jsonProp

prop_text :: Foo -> Property
prop_text f =
  as (asText f) === Right f

return []
tests :: IO Bool
tests = $quickCheckAll
