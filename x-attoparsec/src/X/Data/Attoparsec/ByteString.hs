{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module X.Data.Attoparsec.ByteString (
    module X
  , sepByByte1
  ) where

import           Data.Attoparsec.ByteString as X

import           Data.Word (Word8)

import           P

-- | Same as sepBy1, but specialized for a single byte separator.
sepByByte1 :: Parser a -> Word8 -> Parser [a]
sepByByte1 p !sep = {-# SCC sepByByte1 #-}
  liftM2' (:) p go
  where
    go = do
      peekWord8 >>= \case
        Just c -> if c == sep
                    then liftM2' (:) (anyWord8 *> p) go
                    else pure []
        Nothing -> pure []
{-# INLINE sepByByte1 #-}
