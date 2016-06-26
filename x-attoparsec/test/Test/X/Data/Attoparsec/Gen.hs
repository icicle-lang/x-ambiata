{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test.X.Data.Attoparsec.Gen where

import qualified Data.ByteString as BS

import           P

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

bsWithout c = fmap BS.pack . listOf $ arbitrary `suchThat` (/= c)
