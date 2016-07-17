{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.X.Data.Vector.Stream.Enum where

import           P

import           System.IO (IO)

import           Test.QuickCheck (Property, (===), quickCheckAll)
import           Test.QuickCheck.Instances ()

import qualified X.Data.Vector.Stream as Stream
import qualified Data.List as List

prop_enumFrom :: Int -> Property
prop_enumFrom i =
  let enum1 = Stream.listOfStream $ Stream.take i $ Stream.enumFrom (0 :: Int)
      enum2 = List.take i [0..]
  in  enum1 === enum2


return []
tests :: IO Bool
tests = $quickCheckAll


