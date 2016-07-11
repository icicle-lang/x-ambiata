{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.X.Data.Vector.Stream.Merge where

import           P

import           System.IO (IO)

import           Test.QuickCheck (Property, (===), quickCheckAll)
import           Test.QuickCheck.Instances ()

import qualified X.Data.Vector.Stream as Stream
import qualified Data.List as List

import Data.Functor.Identity

viaStream2 :: (Stream.Stream Identity a -> Stream.Stream Identity a -> Stream.Stream Identity a) -> [a] -> [a] -> [a]
viaStream2 f xs ys =
  Stream.listOfStream $ f (Stream.streamOfList xs) (Stream.streamOfList ys)

checkMerge :: (Eq a, Show a) => (a -> a -> Stream.MergePullFrom a) -> [a] -> [a] -> Property
checkMerge f xs ys =
  viaStream2 (Stream.merge f) xs ys === Stream.mergeList f xs ys


-- We don't need to worry about whether the input is sorted or not if it acts the same as the list version
prop_mergeOrd :: [Int] -> [Int] -> Property
prop_mergeOrd =
  checkMerge (Stream.mergePullOrd id)

-- Something a little mad - like @zipWith (+)@ with tails
prop_mergeAlwaysSum :: [Int] -> [Int] -> Property
prop_mergeAlwaysSum =
  checkMerge (\a b -> Stream.MergePullBoth (a + b))

-- Ok, the 'real' test
prop_mergeSorted :: [Int] -> [Int] -> Property
prop_mergeSorted xs ys =
 let xs' = List.sort xs
     ys' = List.sort ys
     out = List.sort (xs <> ys)
     fun = Stream.mergePullOrd id
  in viaStream2 (Stream.merge fun) xs' ys' === out


return []
tests :: IO Bool
tests = $quickCheckAll


