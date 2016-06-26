{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.X.Data.Attoparsec.ByteString where

import           Data.Attoparsec.ByteString (parseOnly, notWord8)
import qualified Data.ByteString as BS
import           Data.Word (Word8)

import           P

import           System.IO (IO)

import           Test.QuickCheck
import           Test.X.Data.Attoparsec.Gen

import           X.Data.Attoparsec.ByteString

prop_sepByByte1 :: Word8 -> Property
prop_sepByByte1 s = forAll (listOf1 (bsWithout s)) $ \bss ->
  let bs' = BS.intercalate (BS.pack $ pure s) bss
      r = parseOnly (sepByByte1 (many (notWord8 s)) s) bs' in
  r === Right (BS.unpack <$> bss)

return []
tests :: IO Bool
tests = $quickCheckAll
