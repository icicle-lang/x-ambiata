{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.X.Data.ByteString.Char8 where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import           Disorder.Jack (Args(..), stdArgs, forAllProperties, quickCheckWithResult)
import           Disorder.Jack (Property, (===), gamble, arbitrary, listOf, choose)

import           P

import           System.IO (IO)

import           Test.QuickCheck.Instances ()

import qualified X.Data.ByteString as B
import qualified X.Data.ByteString.Char8 as Char8


prop_asciiToLower_upper :: Property
prop_asciiToLower_upper =
  gamble arbitrary $ \t ->
    let x = T.encodeUtf8 $ T.toLower t in
    x === Char8.asciiToLower x

prop_asciiToLower_idempotent :: Property
prop_asciiToLower_idempotent =
  gamble arbitrary $ \x ->
    Char8.asciiToLower (Char8.asciiToLower x) === Char8.asciiToLower x

prop_asciiToLower_text :: Property
prop_asciiToLower_text =
  gamble ascii $ \x ->
    let x' = T.decodeUtf8 x
        y = Char8.asciiToLower x
        y' = T.encodeUtf8 $ T.toLower x' in
    y' === y
    where
      ascii = fmap B.pack . listOf $ choose (0, 127)

prop_eq1 :: Property
prop_eq1 =
  gamble arbitrary $ \x0 ->
  gamble arbitrary $ \xs ->
    (Char8.eq1 x0 xs)
    ===
    (Char8.pack [x0] == xs)

prop_eq2 :: Property
prop_eq2 =
  gamble arbitrary $ \x0 ->
  gamble arbitrary $ \x1 ->
  gamble arbitrary $ \xs ->
    (Char8.eq2 x0 x1 xs)
    ===
    (Char8.pack [x0, x1] == xs)

prop_eq3 :: Property
prop_eq3 =
  gamble arbitrary $ \x0 ->
  gamble arbitrary $ \x1 ->
  gamble arbitrary $ \x2 ->
  gamble arbitrary $ \xs ->
    (Char8.eq3 x0 x1 x2 xs)
    ===
    (Char8.pack [x0, x1, x2] == xs)

prop_eq4 :: Property
prop_eq4 =
  gamble arbitrary $ \x0 ->
  gamble arbitrary $ \x1 ->
  gamble arbitrary $ \x2 ->
  gamble arbitrary $ \x3 ->
  gamble arbitrary $ \xs ->
    (Char8.eq4 x0 x1 x2 x3 xs)
    ===
    (Char8.pack [x0, x1, x2, x3] == xs)

prop_eq5 :: Property
prop_eq5 =
  gamble arbitrary $ \x0 ->
  gamble arbitrary $ \x1 ->
  gamble arbitrary $ \x2 ->
  gamble arbitrary $ \x3 ->
  gamble arbitrary $ \x4 ->
  gamble arbitrary $ \xs ->
    (Char8.eq5 x0 x1 x2 x3 x4 xs)
    ===
    (Char8.pack [x0, x1, x2, x3, x4] == xs)

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 1000 })
