{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
module X.Data.Conduit.Binary (
    BinaryException(..)
  , slurp
  , slurpWithBuffer
  , slurpHandle
  , slurpHandleWithBuffer
  , sepByByteBounded
  ) where

import qualified Control.Exception as E
import           Control.Monad (unless)
import           Control.Monad.Trans (lift)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Conduit
import           Data.Typeable (Typeable)
import           Data.Word (Word8)

import           System.IO

-- This is based on `conduit-extra` version 1.1.7.3
-- Data.Conduit.Binary
--   sourceFileRange
--   souceHandleRange
--
-- Ideally this will be replaced by conduit-extra in a future release
-- based on (https://github.com/snoyberg/conduit/pull/213)
--

slurp :: MonadResource m
         => FilePath
         -> Integer -- ^ Offset
         -> Maybe Integer -- ^ Maximum count
         -> Producer m BS.ByteString
slurp fp offset count' =
  slurpWithBuffer fp offset count' (1024 * 1024)

slurpWithBuffer :: MonadResource m
                     => FilePath
                     -> Integer -- ^ Offset
                     -> Maybe Integer -- ^ Maximum count
                     -> Int -- ^ Buffer size
                     -> Producer m BS.ByteString
slurpWithBuffer fp offset count' buffer = bracketP
  (openBinaryFile fp ReadMode)
  hClose
  (\h -> slurpHandleWithBuffer h offset count' buffer)

slurpHandle :: MonadIO m
         => Handle
         -> Integer -- ^ Offset
         -> Maybe Integer -- ^ Maximum count
         -> Producer m BS.ByteString
slurpHandle handle offset count' =
  slurpHandleWithBuffer handle offset count' (1024 * 1024)

slurpHandleWithBuffer :: MonadIO m
                      => Handle
                      -> Integer -- ^ Offset
                      -> Maybe Integer -- ^ Maximum count
                      -> Int -- ^ Buffer size
                      -> Producer m BS.ByteString
slurpHandleWithBuffer handle offset count' buffer = do
  liftIO $ hSeek handle AbsoluteSeek offset
  case count' of
    Nothing -> pullUnlimited
    Just c -> pullLimited (fromInteger c)
  where
    pullUnlimited = do
        bs <- liftIO $ BS.hGetSome handle buffer
        if BS.null bs
            then return ()
            else do
                yield bs
                pullUnlimited

    pullLimited c = do
        bs <- liftIO $ BS.hGetSome handle (min c $ buffer)
        let c' = c - BS.length bs
        if BS.null bs
          then return ()
          else do
            yield bs
            pullLimited c'

data BinaryException = LengthExceeded Int
  deriving (Eq, Show, Typeable)

instance E.Exception BinaryException

sepByByteBounded :: MonadThrow m => Word8 -> Int -> Conduit ByteString m ByteString
sepByByteBounded s n =
  awaitBytes 0 BS.empty
    where
      awaitBytes len buf = await >>= maybe (finish buf) (process len buf)

      finish buf = unless (BS.null buf) (yield buf)

      process len buf bs =
        let (line, rest) = BS.break (== s) bs
            len' = len + BS.length line in
        if len' > n
          then lift $ monadThrow (LengthExceeded n)
          else case BS.uncons rest of
            Just (_, rest') ->
              yield (buf `BS.append` line) >> process 0 BS.empty rest'
            _ ->
              awaitBytes len' $ buf `BS.append` bs
