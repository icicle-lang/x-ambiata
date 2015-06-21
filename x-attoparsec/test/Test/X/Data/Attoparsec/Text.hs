{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.X.Data.Attoparsec.Text where

import qualified Data.Text as T
import Data.Monoid
import           System.IO ()

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Data.Attoparsec.Text
import           X.Data.Attoparsec.Text

prop_positiveIntegerParserSuccess :: Positive Integer -> Property
prop_positiveIntegerParserSuccess (Positive i) =
  parseStringEither positiveIntegerParser (show i) === Right i

prop_positiveIntegerParserFail :: String -> Property
prop_positiveIntegerParserFail s =
  parseStringEither positiveIntegerParser ('x' : show s) === Left "digit: Failed reading: satisfy"

prop_startsWith :: T.Text -> T.Text -> Property
prop_startsWith prefix t =
  (startsWith (string prefix) (prefix <> t) === True) .&&.
  (startsWith (string ("a" <> prefix)) ("b" <> prefix <> t) === False)

return []
tests :: IO Bool
tests = $quickCheckAll
