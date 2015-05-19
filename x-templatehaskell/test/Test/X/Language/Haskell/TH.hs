{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.X.Language.Haskell.TH (tests) where

import           Control.Monad (return)

import           Data.Bool
import           Data.Int
import           Data.Function
import           System.IO

import           Test.QuickCheck

import           Test.X.Framework.Language.Haskell.TH

{-  COMPILATION TESTS -}

prop_maybe :: Bool
prop_maybe =
  const True ([qint|0|] :: Int)

prop_either :: Bool
prop_either =
  const True ([qint'|1337|] :: Int)

return []
tests :: IO Bool
tests = $quickCheckAll
