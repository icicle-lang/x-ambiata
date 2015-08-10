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
  parseOnly positiveIntegerParser (T.pack . show $ i) === Right i

prop_positiveIntegerParserFail :: String -> Property
prop_positiveIntegerParserFail s =
  parseOnly positiveIntegerParser ("x" <> (T.pack . show) s) === Left "digit: Failed reading: satisfy"

prop_positiveIntParserSuccess :: Positive Int -> Property
prop_positiveIntParserSuccess (Positive i) =
  parseOnly positiveIntParser (T.pack . show $ i) === Right i

prop_positiveIntParserFail :: String -> Property
prop_positiveIntParserFail s =
  parseOnly positiveIntParser ("x" <> (T.pack . show) s) === Left "digit: Failed reading: satisfy"

prop_startsWith :: T.Text -> T.Text -> Property
prop_startsWith prefix t =
  (startsWith (string prefix) (prefix <> t) === True) .&&.
  (startsWith (string ("a" <> prefix)) ("b" <> prefix <> t) === False)

prop_eitherText :: T.Text -> Property
prop_eitherText =
  sameParsers (eitherText Right) takeText

prop_eitherTextFailed :: T.Text -> Property
prop_eitherTextFailed t =
  sameParsers (eitherText Left) (fail (T.unpack t) :: Parser T.Text) t


----------
-- HELPERS
----------

sameParsers :: (Eq a, Show a) => Parser a -> Parser a -> T.Text -> Property
sameParsers p1 p2 t = parseOnly p1 t === parseOnly p2 t

--
return []
tests :: IO Bool
tests = $quickCheckAll
