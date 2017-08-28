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


prop_concatRights_works :: Property
prop_concatRights_works =
  QC.forAll QC.arbitrary $ \ (xss :: [Either Char Int]) -> do
    let xs = DL.map (either Left (Right . abs)) xss
    let ys = runIC $ CL.sourceList xs =$= CL.map expandRight =$= concatRights $$ CL.consume
    QC.counterexample "lefts" (DL.length (lefts ys) === DL.length (lefts xs))
      .&&. QC.counterexample "sum" (sum (rights ys) === sum (rights xs))
  where
    expandRight :: Either Char Int -> Either Char [Int]
    expandRight (Left x) = Left x
    expandRight (Right x) = Right $ replicate x 1

prop_mapRightConcat_works :: Property
prop_mapRightConcat_works =
  QC.forAll QC.arbitrary $ \ (xss :: [Either Char Int]) -> do
    let xs = DL.map (either Left (Right . abs)) xss
    let ys = runIC $ CL.sourceList xs =$= mapRightConcat expandInt $$ CL.consume
    QC.counterexample "lefts" (DL.length (lefts ys) === DL.length (lefts xs))
      .&&. QC.counterexample "sum" (sum (rights ys) === sum (rights xs))
  where
    expandInt :: Int -> [Int]
    expandInt x = replicate x 1

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

prop_groupRightBy_works :: Property
prop_groupRightBy_works =
  QC.forAll (DL.sortBy cmpRight <$> QC.arbitrary) $ \ (xs :: [Either Char Int]) -> do
    let ys = runIC $ CL.sourceList xs =$= groupRightBy (==) $$ CL.consume
    QC.counterexample "rights" (rights ys === DL.group (rights xs))
      .&&. QC.counterexample "lefts" (lefts ys == lefts xs)
  where
    cmpRight (Left _) _ = EQ
    cmpRight _ (Left _) = EQ
    cmpRight (Right a) (Right b) = compare a b

-- -----------------------------------------------------------------------------

return []
tests :: IO Bool
tests = $forAllProperties $ QC.quickCheckWithResult (QC.stdArgs { QC.maxSuccess = 1000 })
