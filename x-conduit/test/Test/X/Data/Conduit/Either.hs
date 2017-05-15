{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.X.Data.Conduit.Either
  ( tests
  ) where

import           Control.Monad.Identity (Identity, runIdentity)

import           Data.Conduit (ConduitM, (=$=), ($$))
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import           Data.Either (lefts, rights)
import qualified Data.List as DL
import           Data.Maybe (mapMaybe)
import           Data.Void (Void)

import           Disorder.Core.IO (testIO)

import           System.IO (Handle)
import qualified System.IO as IO
import           System.IO.Temp (withTempFile)

import           Test.QuickCheck (Property, (===), (.&&.), forAllProperties)
import qualified Test.QuickCheck as QC
import           Text.Read (readMaybe)

import           X.Data.Conduit.Either


runIC :: ConduitM () Void Identity c -> c
runIC = runIdentity . C.runConduit


prop_bindEither_works :: Property
prop_bindEither_works =
  QC.forAll QC.arbitrary $ \ (xs :: [Either Char Int]) -> do
    let ys = runIC $ CL.sourceList xs =$= bindEither errOdd $$ CL.consume
    QC.counterexample "length" (DL.length ys === DL.length xs)
      .&&. QC.counterexample "even" (QC.property (DL.all even $ rights ys))
  where
    errOdd :: Int -> Either Char Int
    errOdd i = if odd i then Left 'o' else Right i

prop_mapRightE_works :: Property
prop_mapRightE_works =
  QC.forAll QC.arbitrary $ \ (xs :: [Either Char Int]) -> do
    let ys = runIC $ CL.sourceList xs =$= mapRightE charToString errOdd $$ CL.consume
    QC.counterexample "length" (DL.length ys === DL.length xs)
      .&&. QC.counterexample "even" (QC.property (DL.all even $ rights ys))
  where
    errOdd :: Int -> Either String Int
    errOdd i = if odd i then Left (show i) else Right i

    charToString :: Char -> String
    charToString c = show c


prop_mapRightM_works :: Property
prop_mapRightM_works =
  QC.forAll QC.arbitrary $ \ (xs :: [Either Char Int]) -> testIO $
    withTempFile "/tmp/" "jumbuck-test" $ \fpath handle -> do
      ys <- C.runConduit $ CL.sourceList xs =$= mapRightM_ (writeInt handle) $$ CL.consume
      IO.hClose handle
      ints <- readInts fpath
      pure $ QC.counterexample "lefts" (lefts ys === lefts xs)
                .&&. QC.counterexample "rights" (rights ys === rights xs)
                .&&. QC.counterexample "ints" (ints === rights xs)
  where
    writeInt :: Handle -> Int -> IO (Either e ())
    writeInt h i = Right <$> IO.hPrint h i

    readInts :: FilePath -> IO [Int]
    readInts fp = mapMaybe readMaybe . DL.lines <$> IO.readFile fp

-- -----------------------------------------------------------------------------

return []
tests :: IO Bool
tests = $forAllProperties $ QC.quickCheckWithResult (QC.stdArgs { QC.maxSuccess = 1000 })
