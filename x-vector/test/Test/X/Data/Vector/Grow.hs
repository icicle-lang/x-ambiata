{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.X.Data.Vector.Grow where

import           Control.Monad.ST (runST)

import           P

import           System.IO (IO)

import           Test.QuickCheck (Property, (===), quickCheckAll)
import           Test.QuickCheck.Instances ()

import qualified X.Data.Vector.Grow as Grow
import qualified X.Data.Vector.Unboxed as Unboxed


prop_add_freeze :: [Int] -> Property
prop_add_freeze xs0 =
  let
    xs =
      Unboxed.fromList xs0
  in
    runST $ do
      g <- Grow.new 0
      Unboxed.mapM_ (Grow.add g) xs
      ys <- Grow.freeze g
      pure $
        xs === ys

prop_append_freeze :: [Int] -> [Int] -> Property
prop_append_freeze xs0 ys0 =
  let
    xs =
      Unboxed.fromList xs0

    ys =
      Unboxed.fromList ys0
  in
    runST $ do
      g <- Grow.new 0
      Grow.append g xs
      Grow.append g ys
      zs <- Grow.freeze g
      pure $
        xs <> ys === zs

prop_append_vs_add :: [Int] -> Property
prop_append_vs_add xs0 =
  let
    xs =
      Unboxed.fromList xs0

    ys :: Unboxed.Vector Int
    ys =
      runST $ do
        g <- Grow.new 0
        Grow.append g xs
        Grow.freeze g

    zs :: Unboxed.Vector Int
    zs =
      runST $ do
        g <- Grow.new 0
        Unboxed.mapM_ (Grow.add g) xs
        Grow.freeze g
  in
    ys === zs

return []
tests :: IO Bool
tests = $quickCheckAll
