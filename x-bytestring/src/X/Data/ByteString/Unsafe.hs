{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
module X.Data.ByteString.Unsafe (
    unsafeSplits
  , unsafeSlice

  , module Data.ByteString.Unsafe
  ) where

import           Data.ByteString.Internal (ByteString(..))
import           Data.ByteString.Unsafe
import qualified Data.Vector.Generic as Generic

import           P


data IdxOff =
  IdxOff !Int !Int

unsafeSplits :: (Generic.Vector va a, Generic.Vector vn Int) => (ByteString -> a) -> ByteString -> vn Int -> va a
unsafeSplits f bs sizes =
  let
    loop (IdxOff idx off) =
      let
        !len =
          Generic.unsafeIndex sizes idx
      in
        Just (f $! unsafeSlice off len bs, IdxOff (idx + 1) (off + len))
  in
    Generic.unfoldrN (Generic.length sizes) loop (IdxOff 0 0)
{-# INLINE unsafeSplits #-}

unsafeSlice :: Int -> Int -> ByteString -> ByteString
unsafeSlice !off !len (PS ptr poff _) =
  PS ptr (poff + off) len
{-# INLINE unsafeSlice #-}
