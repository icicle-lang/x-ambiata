{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.X.Control.Monad.Catch where

import           Control.Applicative ((<$>), pure)
import           Control.Monad (return)

import           Data.Bool (Bool)
import           Data.Either (Either(..))
import           Data.Eq ((/=))
import           Data.Function (($), (.), const)
import           Data.Text (Text)

import           Disorder.Core.IO (testIO)

import           System.IO (IO)

import           Test.QuickCheck (Property, (==>), (===), quickCheckAll)
import           Test.QuickCheck.Instances ()

import           X.Control.Monad.Catch


prop_bracketF :: [Text] -> Text -> Property
prop_bracketF l action = action /= "" ==> testIO $ do
  let after' = const . pure $ Right ()
  let action' = \l' -> pure $ action : l'
  (=== action : l) <$> bracketF (pure l) after' action'

prop_bracketF_failure :: [Text] -> Text -> Text -> Property
prop_bracketF_failure l action failure = action /= "" ==> testIO $ do
  let after' = const . pure $ Left failure
  let action' = \_ -> pure action
  (=== failure) <$> bracketF (pure l) after' action'


return []
tests :: IO Bool
tests = $quickCheckAll
