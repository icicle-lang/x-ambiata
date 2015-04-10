{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module X.Test.Text.Toml where

import           Control.Applicative
import           Control.Monad

import           Data.Function
import           Data.Bool
import           Data.Ord

import           Orphanarium.Lens

import           System.IO

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Text.Toml.Types

import           X.Text.Toml


prop_NTValue :: Property
prop_NTValue =
  property $ prismSymmetry _NTValue

prop_NTable :: Property
prop_NTable =
  property $ prismSymmetry _NTable

prop_NTArray :: Property
prop_NTArray =
  property $ prismSymmetry _NTArray

prop_VString :: Property
prop_VString =
  property $ prismSymmetry _VString

prop_VInteger :: Property
prop_VInteger =
  property $ prismSymmetry _VInteger

prop_VFloat :: Property
prop_VFloat =
  property $ prismSymmetry _VFloat

prop_VBoolean :: Property
prop_VBoolean =
  property $ prismSymmetry _VBoolean

prop_VDatetime :: Property
prop_VDatetime =
  property $ prismSymmetry _VDatetime

prop_VArray :: Property
prop_VArray =
  property $ prismSymmetry _VArray

instance Arbitrary TValue where
  arbitrary = oneof [
      VString <$> arbitrary
    , VInteger <$> arbitrary
    , VFloat <$> arbitrary
    , VBoolean <$> arbitrary
    , VDatetime <$> arbitrary
    , sized $ \n -> resize (min n 10) $ VArray <$> arbitrary
    ]

instance Arbitrary Node where
  arbitrary = sized $ \n -> resize (min n 2) $ oneof [
      NTValue <$> arbitrary
    , NTable <$> arbitrary
    , NTArray <$> arbitrary
    ]

return []
tests :: IO Bool
tests = $quickCheckAll
