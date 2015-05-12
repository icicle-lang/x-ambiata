{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module X.Data.Conduit.Binary (
    slurp
  , slurpWithBuffer
  , slurpHandle
  , slurpHandleWithBuffer
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource

import qualified Data.ByteString as BS
import           Data.Conduit

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
