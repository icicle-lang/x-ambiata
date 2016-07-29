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

return []
tests :: IO Bool
tests = $quickCheckAll
