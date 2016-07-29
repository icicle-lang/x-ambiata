{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.X.Data.Vector.Ref where

import           Control.Monad.ST (runST)

import qualified Data.Vector.Unboxed.Mutable as MUnboxed

import           P

import           System.IO (IO)

import           Test.QuickCheck (Property, (===), quickCheckAll)
import           Test.QuickCheck.Instances ()

import           X.Data.Vector.Ref


prop_read_after_new :: Int -> Property
prop_read_after_new x =
  runST $ do
    (ref :: Ref MUnboxed.MVector s Int) <- newRef x
    y <- readRef ref
    pure $
      x === y

prop_read_after_write :: Int -> Property
prop_read_after_write x =
  runST $ do
    (ref :: Ref MUnboxed.MVector s Int) <- newRef 0
    writeRef ref x
    y <- readRef ref
    pure $
      x === y

return []
tests :: IO Bool
tests = $quickCheckAll
