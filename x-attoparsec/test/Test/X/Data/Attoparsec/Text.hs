{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.X.Data.Attoparsec.Text where

import qualified Data.Text as T
import           Data.Monoid

import           Disorder.Core.UniquePair (UniquePair(..))

import           System.IO ()

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Test.X.Data.Attoparsec.Arbitrary

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
  sameParsers (takeText >>= eitherText . Right) takeText

prop_eitherTextFailed :: T.Text -> Property
prop_eitherTextFailed t =
  sameParsers (takeText >>= eitherText . Left) (fail (T.unpack t) :: Parser T.Text) t

prop_manyAnd' :: UniquePair NEText -> Property
prop_manyAnd' (UniquePair (NEText a) (NEText b)) = forAll (arbitrary `suchThat` (>= 1) :: Gen Int) $ \n ->
  let as = replicate n a
      ab = T.concat as <> b
      p = manyAnd' (string a) (string b)
      r = parseOnly p ab in
  r === Right (as, b)

----------
-- HELPERS
----------

sameParsers :: (Eq a, Show a) => Parser a -> Parser a -> T.Text -> Property
sameParsers p1 p2 t = parseOnly p1 t === parseOnly p2 t

--
return []
tests :: IO Bool
tests = $quickCheckAll
