{-# LANGUAGE NoImplicitPrelude #-}
module X.Data.ByteString.Char8 (
    asciiToLower

  , eq1
  , eq2
  , eq3
  , eq4
  , eq5

  , module Data.ByteString.Char8
  ) where

import           Data.Bits ((.|.))
import           Data.ByteString.Char8
import           Data.Char (ord)

import           P

import qualified X.Data.ByteString as B


-- | Convert ASCII uppercase letters to lowercase. Other bytes are not
-- affected. This is much, much faster than 'Data.Text.toLower', but obviously
-- doesn't work on UTF-8 beyond its ASCII subset.
asciiToLower :: ByteString -> ByteString
asciiToLower = {-# SCC asciiToLower #-} B.map lower
  where
    lower w
      | w >= 0x41 && w <= 0x5a = w .|. 0x20
      | otherwise              = w
{-# INLINABLE asciiToLower #-}

-- | Fast equality for a 1-byte string.
eq1 :: Char -> ByteString -> Bool
eq1 x0 =
  B.eq1
    (fromIntegral $ ord x0)
{-# INLINE eq1 #-}

-- | Fast equality for a 2-byte string.
eq2 :: Char -> Char -> ByteString -> Bool
eq2 x0 x1 =
  B.eq2
    (fromIntegral $ ord x0)
    (fromIntegral $ ord x1)
{-# INLINE eq2 #-}

-- | Fast equality for a 3-byte string.
eq3 :: Char -> Char -> Char -> ByteString -> Bool
eq3 x0 x1 x2 =
  B.eq3
    (fromIntegral $ ord x0)
    (fromIntegral $ ord x1)
    (fromIntegral $ ord x2)
{-# INLINE eq3 #-}

-- | Fast equality for a 4-byte string.
eq4 :: Char -> Char -> Char -> Char -> ByteString -> Bool
eq4 x0 x1 x2 x3 =
  B.eq4
    (fromIntegral $ ord x0)
    (fromIntegral $ ord x1)
    (fromIntegral $ ord x2)
    (fromIntegral $ ord x3)
{-# INLINE eq4 #-}

-- | Fast equality for a 5-byte string.
eq5 :: Char -> Char -> Char -> Char -> Char -> ByteString -> Bool
eq5 x0 x1 x2 x3 x4 =
  B.eq5
    (fromIntegral $ ord x0)
    (fromIntegral $ ord x1)
    (fromIntegral $ ord x2)
    (fromIntegral $ ord x3)
    (fromIntegral $ ord x4)
{-# INLINE eq5 #-}
