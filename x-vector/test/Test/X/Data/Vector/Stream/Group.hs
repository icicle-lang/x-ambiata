{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.X.Data.Vector.Stream.Group where

import           Control.Monad.ST (runST)

import           P

import           System.IO (IO)

import           Test.QuickCheck (Property, quickCheckAll)
import           Test.QuickCheck ((===), (==>), counterexample, property, conjoin)
import           Test.QuickCheck.Instances ()

import qualified X.Data.Vector.Stream as Stream
import qualified X.Data.Vector.Unboxed as Unboxed


prop_chunksOf :: Int -> [Int] -> Property
prop_chunksOf n xs0 =
  n > 0 ==>
  runST $ do
    let
      yss0 =
        Stream.chunksOf n $
        Stream.fromList xs0

    yss <- Stream.toList yss0

    let
      subprop_same_values =
        [ concatMap Unboxed.toList yss === xs0 ]

      subprop_at_most_n =
        fmap (\ys -> property $ Unboxed.length ys <= n) yss

      subprop_no_empty =
        fmap (\ys -> property $ not (Unboxed.null ys)) yss

    pure . counterexample ("chunks = " <> show yss) . conjoin $
      subprop_same_values <>
      subprop_at_most_n <>
      subprop_no_empty

return []
tests :: IO Bool
tests = $quickCheckAll
