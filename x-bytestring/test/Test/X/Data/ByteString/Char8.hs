{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.X.Data.ByteString.Char8 where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import           P

import           System.IO (IO)

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           X.Data.ByteString.Char8

prop_asciiToLower_upper t =
  let x = T.encodeUtf8 $ T.toLower t in
  x === asciiToLower x

prop_asciiToLower_idempotent x =
  asciiToLower (asciiToLower x) === asciiToLower x

prop_asciiToLower_text = forAll ascii $ \x ->
  let x' = T.decodeUtf8 x
      y = asciiToLower x
      y' = T.encodeUtf8 $ T.toLower x' in
  y' === y
  where
    ascii = fmap BS.pack . listOf $ choose (0, 127)

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 1000 })
