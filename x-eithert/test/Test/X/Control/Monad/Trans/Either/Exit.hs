{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.X.Control.Monad.Trans.Either.Exit where

import           Control.Applicative (pure)
import           Control.Monad ((>>=), return)

import           Data.Function (($), id)
import           Data.Bool (Bool)
import           Data.Int (Int)

import           System.IO (IO)

import           Test.QuickCheck (Property, ioProperty, (===), quickCheckAll)
import           Test.QuickCheck.Instances ()

import           X.Control.Monad.Trans.Either.Exit


prop_orDie :: Int -> Property
prop_orDie n =
  ioProperty $ orDie id (pure n) >>= \r -> pure $ r === n


return []
tests :: IO Bool
tests = $quickCheckAll
