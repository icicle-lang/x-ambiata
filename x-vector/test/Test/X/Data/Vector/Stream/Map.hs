{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.X.Data.Vector.Stream.Map where

import           P

import           System.IO (IO)

import           Test.QuickCheck (Property, (===), quickCheckAll)
import           Test.QuickCheck.Instances ()

import qualified X.Data.Vector.Stream as Stream

prop_mapAccumulateIndexed :: [Int] -> Property
prop_mapAccumulateIndexed xs =
  let xs' = Stream.streamOfList xs
      ix1 = Stream.listOfStream (Stream.mapAccumulate (\ix e -> (ix + 1, (ix,e))) 0 xs')
      ix2 = Stream.listOfStream (Stream.indexed xs')
  in  ix1 === ix2


return []
tests :: IO Bool
tests = $quickCheckAll


