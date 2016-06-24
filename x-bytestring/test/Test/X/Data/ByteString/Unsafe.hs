{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.X.Data.ByteString.Unsafe where

import qualified Data.ByteString as B
import qualified Data.Vector as Boxed

import           Disorder.Jack (Property, quickCheckAll)
import           Disorder.Jack ((===), gamble, arbitrary, listOf)

import           P

import           System.IO (IO)

import           Test.QuickCheck.Instances ()

import           X.Data.ByteString.Unsafe


prop_slice_substring :: Property
prop_slice_substring =
  gamble arbitrary $ \pre ->
  gamble arbitrary $ \inf ->
  gamble arbitrary $ \suf ->
    let
      bs = pre <> inf <> suf
      off = B.length pre
      len = B.length inf
    in
      inf === unsafeSlice off len bs

prop_split_strings :: Property
prop_split_strings =
  gamble (listOf arbitrary) $ \bss0_list ->
    let
      bs =
        B.concat bss0_list

      bss0 =
        Boxed.fromList bss0_list

      bss =
        unsafeSplits id bs (fmap B.length bss0)
    in
      bss0 === bss

return []
tests :: IO Bool
tests =
  $quickCheckAll
