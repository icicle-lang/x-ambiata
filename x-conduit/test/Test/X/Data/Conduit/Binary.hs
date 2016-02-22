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
import           Data.Char (ord)
import           Data.Int
import           Data.Maybe
import           Data.ByteString
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Conduit
import           Data.Conduit.Binary
import           Data.Conduit.List (consume)
import qualified Data.List as L
import           Disorder.Core.IO

import           Prelude ((-), toInteger, fromIntegral, (/=))

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

prop_sepByByteBounded :: Property
prop_sepByByteBounded = forAll (arbitrary `suchThat` (elem nl)) $ \bs -> 
    testIO $ withSystemTempDirectory "conduit" $ \p -> do
      let fp = p </> "file"
      BS.writeFile fp bs
      r1 <- runResourceT $ slurp fp 0 Nothing =$= sepByByteBounded nl 65536 $$ sinkLbs
      r2 <- LBS.filter (/= nl) <$> LBS.readFile fp
      pure $ r1 === r2
  where
    nl = fromIntegral $ ord '\n'
  

prop_sepByByteBounded_lines :: Property
prop_sepByByteBounded_lines =
  forAll arbitrary $ \bs -> testIO $
    withSystemTempDirectory "conduit" $ \p -> do
      let f = p </> "file"
      BS.writeFile f bs
      r1 <- runResourceT $ (slurp f 0 Nothing) =$= sepByByteBounded 0x0a 16384 $$ sinkLbs
      r2 <- runResourceT $ (slurp f 0 Nothing) =$= lines $$ sinkLbs
      pure $ r1 === r2

-- Data for this test was randomly generated.
prop_sepByByteBounded_psv :: Property
prop_sepByByteBounded_psv = testIO $ do
  let fp = "test/data/test.psv"
  r1 <- runResourceT $ slurp fp 0 Nothing =$= sepByByteBounded nl 65536 $$ consume
  pure $ L.length r1 === 200
  where
    nl = fromIntegral $ ord '\n'


return []
tests :: IO Bool
tests = $quickCheckAll
