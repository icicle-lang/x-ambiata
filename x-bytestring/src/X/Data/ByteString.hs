{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
module X.Data.ByteString (
    eq1
  , eq2
  , eq3
  , eq4
  , eq5

  , module Data.ByteString
  ) where

import           Data.Bits
import           Data.ByteString
import           Data.ByteString.Internal (ByteString(..), accursedUnutterablePerformIO)
import           Data.Word (Word8, Word16, Word32)

import           Foreign.ForeignPtr (withForeignPtr)
import           Foreign.Storable (peekByteOff)

import           P


-- | Fast equality for a 1-byte string.
eq1 :: Word8 -> ByteString -> Bool
eq1 x0 (PS fp off len) =
  let
    ok =
      accursedUnutterablePerformIO $! withForeignPtr fp $! \ptr -> do
        !y0 <- peekByteOff ptr off
        pure $!
          x0 == y0
  in
    len == 1 && ok
{-# INLINE eq1 #-}

-- | Fast equality for a 2-byte string.
eq2 :: Word8 -> Word8 -> ByteString -> Bool
eq2 x0 x1 (PS fp off len) =
  let
    x01 :: Word16
    !x01 =
      (fromIntegral x0) .|.
      (fromIntegral x1 `shiftL` 8)

    ok =
      accursedUnutterablePerformIO $! withForeignPtr fp $! \ptr -> do
        !y01 <- peekByteOff ptr off
        pure $!
          x01 == y01
  in
    len == 2 && ok
{-# INLINE eq2 #-}

-- | Fast equality for a 3-byte string.
eq3 :: Word8 -> Word8 -> Word8 -> ByteString -> Bool
eq3 x0 x1 x2 (PS fp off len) =
  let
    x01 :: Word16
    !x01 =
      (fromIntegral x0) .|.
      (fromIntegral x1 `shiftL` 8)

    ok =
      accursedUnutterablePerformIO $! withForeignPtr fp $! \ptr -> do
        !y01 <- peekByteOff ptr off
        !y2 <- peekByteOff ptr (off + 2)
        pure $!
          x01 == y01 &&
          x2 == y2
  in
    len == 3 && ok
{-# INLINE eq3 #-}

-- | Fast equality for a 4-byte string.
eq4 :: Word8 -> Word8 -> Word8 -> Word8 -> ByteString -> Bool
eq4 x0 x1 x2 x3 (PS fp off len) =
  let
    x0123 :: Word32
    !x0123 =
      (fromIntegral x0) .|.
      (fromIntegral x1 `shiftL` 8) .|.
      (fromIntegral x2 `shiftL` 16) .|.
      (fromIntegral x3 `shiftL` 16)

    ok =
      accursedUnutterablePerformIO $! withForeignPtr fp $! \ptr -> do
        !y0123 <- peekByteOff ptr off
        pure $!
          x0123 == y0123
  in
    len == 4 && ok
{-# INLINE eq4 #-}

-- | Fast equality for a 5-byte string.
eq5 :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> ByteString -> Bool
eq5 x0 x1 x2 x3 x4 (PS fp off len) =
  let
    x0123 :: Word32
    !x0123 =
      (fromIntegral x0) .|.
      (fromIntegral x1 `shiftL` 8) .|.
      (fromIntegral x2 `shiftL` 16) .|.
      (fromIntegral x3 `shiftL` 16)

    ok =
      accursedUnutterablePerformIO $! withForeignPtr fp $! \ptr -> do
        !y0123 <- peekByteOff ptr off
        !y4 <- peekByteOff ptr (off + 4)
        pure $!
          x0123 == y0123 &&
          x4 == y4
  in
    len == 5 && ok
{-# INLINE eq5 #-}
