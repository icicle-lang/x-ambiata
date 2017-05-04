{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.X.Data.ByteString where

import           Disorder.Jack (Property, quickCheckAll)
import           Disorder.Jack ((===), gamble, arbitrary)

import           P

import           System.IO (IO)

import           Test.QuickCheck.Instances ()

import qualified X.Data.ByteString as B


prop_eq1 :: Property
prop_eq1 =
  gamble arbitrary $ \x0 ->
  gamble arbitrary $ \xs ->
    (B.eq1 x0 xs)
    ===
    (B.pack [x0] == xs)

prop_eq2 :: Property
prop_eq2 =
  gamble arbitrary $ \x0 ->
  gamble arbitrary $ \x1 ->
  gamble arbitrary $ \xs ->
    (B.eq2 x0 x1 xs)
    ===
    (B.pack [x0, x1] == xs)

prop_eq3 :: Property
prop_eq3 =
  gamble arbitrary $ \x0 ->
  gamble arbitrary $ \x1 ->
  gamble arbitrary $ \x2 ->
  gamble arbitrary $ \xs ->
    (B.eq3 x0 x1 x2 xs)
    ===
    (B.pack [x0, x1, x2] == xs)

prop_eq4 :: Property
prop_eq4 =
  gamble arbitrary $ \x0 ->
  gamble arbitrary $ \x1 ->
  gamble arbitrary $ \x2 ->
  gamble arbitrary $ \x3 ->
  gamble arbitrary $ \xs ->
    (B.eq4 x0 x1 x2 x3 xs)
    ===
    (B.pack [x0, x1, x2, x3] == xs)

prop_eq5 :: Property
prop_eq5 =
  gamble arbitrary $ \x0 ->
  gamble arbitrary $ \x1 ->
  gamble arbitrary $ \x2 ->
  gamble arbitrary $ \x3 ->
  gamble arbitrary $ \x4 ->
  gamble arbitrary $ \xs ->
    (B.eq5 x0 x1 x2 x3 x4 xs)
    ===
    (B.pack [x0, x1, x2, x3, x4] == xs)

return []
tests :: IO Bool
tests =
  $quickCheckAll
