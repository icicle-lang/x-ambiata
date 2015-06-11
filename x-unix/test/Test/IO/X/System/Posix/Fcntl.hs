{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.IO.X.System.Posix.Fcntl where

import           Data.Bool

import           Control.Monad
import           Data.Function

import           System.IO

import           Test.QuickCheck

prop_foo :: Bool -> Property
prop_foo b =
  b === b

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 10 })
