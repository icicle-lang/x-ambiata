{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.X.Data.Attoparsec.Arbitrary where

import qualified Data.Text as T

import           P

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

newtype NEText =
  NEText {
    unNEText :: T.Text
  } deriving (Eq, Show)

instance Arbitrary NEText where
  arbitrary = fmap NEText $ arbitrary `suchThat` ((> 0) . T.length)

