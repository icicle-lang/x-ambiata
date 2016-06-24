{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module X.Data.Attoparsec.ByteString.Ascii (
    -- * Predicates.
    isAlpha
  , isAlphaLower
  , isAlphaNum
  , isAlphaUpper
  , isDigit
  , isPrintable
  , isWhitespace
  ) where

import qualified Data.Attoparsec.ByteString.Char8 as AB
import           Data.Word

import           P

isAlphaUpper :: Word8 -> Bool
isAlphaUpper w = w >= 0x41 && w <= 0x5a
{-# INLINE isAlphaUpper #-}

isAlphaLower :: Word8 -> Bool
isAlphaLower w = w >= 0x61 && w <= 0x7a
{-# INLINE isAlphaLower #-}

isAlpha :: Word8 -> Bool
isAlpha w = isAlphaUpper w || isAlphaLower w
{-# INLINE isAlpha #-}

isDigit :: Word8 -> Bool
isDigit = AB.isDigit_w8
{-# INLINE isDigit #-}

isAlphaNum :: Word8 -> Bool
isAlphaNum w = isAlpha w || isDigit w
{-# INLINE isAlphaNum #-}

-- | Including space but not including either kind of tab or either kind of
-- newline.
isPrintable :: Word8 -> Bool
isPrintable w = w >= 0x20 && w < 0x7f
{-# INLINE isPrintable #-}

-- | Space, both newlines, both tabs.
isWhitespace :: Word8 -> Bool
isWhitespace = AB.isSpace_w8
{-# INLINE isWhitespace #-}
