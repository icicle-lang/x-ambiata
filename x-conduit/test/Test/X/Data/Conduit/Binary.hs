{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.X.Data.Conduit.Binary (tests) where

import           Control.Monad (return)
import           Control.Monad.Trans.Resource
import           Control.Applicative

import           Data.Function
import           Data.Bool
import           Data.Int
import           Data.Maybe
import           Data.ByteString
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Conduit
import           Data.Conduit.Binary

import           Disorder.Core.IO

import           Prelude ((-), toInteger)

import           System.IO
import           System.IO.Temp
import           System.FilePath

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           X.Data.Conduit.Binary

prop_slurp :: Property
prop_slurp =
  forAll arbitrary $ \bs ->
    forAll (choose (100, 4096) :: Gen Int) $ \buffer ->
      forAll (choose (0, length bs)) $ \offset ->
        forAll (choose (0, (length bs) - offset)) $ \chunk -> testIO $ do
          withSystemTempDirectory "conduit" $ \p -> do
            let f = p </> "file"
            BS.writeFile f bs
            r <- runResourceT $ (slurpWithBuffer f (toInteger offset) (Just $ toInteger chunk) buffer) $$ sinkLbs
            pure $ r === (LBS.fromChunks . return . BS.take chunk . BS.drop offset $ bs)

return []
tests :: IO Bool
tests = $quickCheckAll
