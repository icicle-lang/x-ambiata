{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.X.Text.Show where

import           Data.String (String)
import qualified Data.List as List

import           GHC.Generics (Generic)

import           P

import           System.IO (IO)

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           X.Text.Show


data Foo =
    Foo
  | Bar Bar
  | Baz Baz
    deriving (Generic)

newtype Bar =
  MkBar {
      unBar :: Int
    } deriving (Generic)

data Baz =
  MkBaz {
      bazInt :: Int
    , bazDouble :: Double
    , bazBar :: Bar
    } deriving (Generic)

instance Show Foo where
  showsPrec =
    gshowsPrec

instance Show Bar where
  showsPrec =
    gshowsPrec

instance Show Baz where
  showsPrec =
    gshowsPrec

instance Arbitrary Foo where
  arbitrary =
    oneof [
        pure Foo
      , Bar <$> arbitrary
      , Baz <$> arbitrary
      ]

  shrink =
    genericShrink

instance Arbitrary Bar where
  arbitrary =
    MkBar <$> arbitrary

  shrink =
    genericShrink

instance Arbitrary Baz where
  arbitrary =
    MkBaz <$> arbitrary <*> arbitrary <*> arbitrary

  shrink =
    genericShrink

prop_no_equals :: Foo -> Property
prop_no_equals =
  mustNotContain "="

prop_no_lbrace :: Foo -> Property
prop_no_lbrace =
  mustNotContain "{"

prop_no_rbrace :: Foo -> Property
prop_no_rbrace =
  mustNotContain "}"

mustNotContain :: Show a => String -> a -> Property
mustNotContain needle x =
  let
    s =
      show x

    msg =
      "\n  Derived show instance contained traces of record labels." <>
      "\n" <>
      "\n  Found " <> show needle <> " in show output:" <>
      "\n" <>
      "\n    " <> s
  in
    counterexample msg . property . not $
      List.isInfixOf needle s

return []
tests :: IO Bool
tests = $quickCheckAll
