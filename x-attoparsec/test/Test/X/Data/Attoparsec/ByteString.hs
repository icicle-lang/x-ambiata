{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.X.Data.Attoparsec.ByteString where

import           System.IO ()

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           X.Data.Attoparsec.ByteString

prop_positiveIntegerParserSuccess :: Positive Integer -> Property
prop_positiveIntegerParserSuccess (Positive i) =
  parseStringEither positiveIntegerParser (show i) === Right i

prop_positiveIntegerParserFail :: String -> Property
prop_positiveIntegerParserFail s =
  parseStringEither positiveIntegerParser ('x' : show s) === Left "digit: Failed reading: satisfyWith"

return []
tests :: IO Bool
tests = $quickCheckAll
