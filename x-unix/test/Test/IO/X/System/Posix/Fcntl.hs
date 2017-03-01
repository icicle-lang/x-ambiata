{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.IO.X.System.Posix.Fcntl where

import           Data.Bool

import           Control.Applicative
import           Control.Monad

import           Data.Function
import           Data.Text

import           Disorder.Core.IO
import           Disorder.Corpus

import           System.IO
import           System.IO.Temp
import           System.FilePath

import           Test.QuickCheck

prop_foo :: Bool -> Property
prop_foo b =
  b === b

prop_allocate :: Property
prop_allocate = forAll (elements southpark) $ \s -> testIO $ do
  withSystemTempDirectory "fcntl" $ \tmp -> do
--    let file = "/tmp/nick/" </> unpack s
    let file = tmp </> unpack s
    withFile file WriteMode $ \h -> do
      hSetFileSize h 100
    r <- withFile file ReadMode $ \h ->
      hFileSize h
    pure $ r === 100

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 10 })
