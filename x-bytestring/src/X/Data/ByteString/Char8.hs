{-# LANGUAGE NoImplicitPrelude #-}
module X.Data.ByteString.Char8 (
    asciiToLower
  ) where

import           Data.Bits ((.|.))
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import           P

-- | Convert ASCII uppercase letters to lowercase. Other bytes are not
-- affected. This is much, much faster than 'Data.Text.toLower', but obviously
-- doesn't work on UTF-8 beyond its ASCII subset.
asciiToLower :: ByteString -> ByteString
asciiToLower = {-# SCC asciiToLower #-} BS.map lower
  where
    lower w
      | w >= 0x41 && w <= 0x5a = w .|. 0x20
      | otherwise              = w
{-# INLINABLE asciiToLower #-}
