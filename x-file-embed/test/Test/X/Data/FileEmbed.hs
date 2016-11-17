{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.X.Data.FileEmbed where

import           Data.ByteString (ByteString)

import           P

import           System.IO (IO, FilePath)

import           Test.QuickCheck (Property, (===), quickCheckAll, once)

import           X.Data.FileEmbed (embedWhen)


files :: [(FilePath, ByteString)]
files =
  $(embedWhen (== "foo.h") "test/example")

prop_check :: Property
prop_check =
  once $
    files === [("foo.h", "int foo (int x);\n")]

return []
tests :: IO Bool
tests =
  $quickCheckAll
