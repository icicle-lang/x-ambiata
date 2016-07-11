{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.X.Data.Vector.Stream.Conversion where

import           P

import           System.IO (IO)

import           Test.QuickCheck (Property, (===), quickCheckAll)
import           Test.QuickCheck.Instances ()

import qualified X.Data.Vector as Boxed
import qualified X.Data.Vector.Stream as Stream

prop_streamOfList :: [Int] -> Property
prop_streamOfList xs =
  Stream.listOfStream (Stream.streamOfList xs) === xs

prop_streamOfVector :: Boxed.Vector Int -> Property
prop_streamOfVector xs =
  Stream.vectorOfStream (Stream.streamOfVector xs) === xs


return []
tests :: IO Bool
tests = $quickCheckAll

