{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module X.Control.Monad.Catch (
    bracketF
  , hushM
  ) where

import           Control.Monad (return, liftM)
import           Control.Monad.Catch (catchAll, throwM, handle)
import           Control.Monad.Catch (Exception(..), SomeException)
import           Control.Monad.Catch (MonadCatch(..), MonadMask(..))

import           Data.Bool (Bool)
import           Data.Either (Either(..), either)
import           Data.Function (($), const, id)
import           Data.Maybe (Maybe(..))


data BracketResult a =
    BracketOk a
  | BracketFailedFinalizerOk SomeException
  | BracketFailedFinalizerError a

-- Bracket where you care about the output of the finalizer. If the finalizer fails
-- with a value level fail, it will return the result of the finalizer.
-- Finalizer:
--  - Left indicates a value level fail.
--  - Right indicates that the finalizer has a value level success, and its results can be ignored.
--
bracketF :: MonadMask m => m a -> (a -> m (Either b c)) -> (a -> m b) -> m b
bracketF a f g =
  mask $ \restore -> do
    a' <- a
    x <- restore (BracketOk `liftM` g a') `catchAll`
           (\ex -> either BracketFailedFinalizerError (const $ BracketFailedFinalizerOk ex) `liftM` f a')
    case x of
      BracketFailedFinalizerOk ex ->
        throwM ex
      BracketFailedFinalizerError b ->
        return b
      BracketOk b -> do
        z <- f a'
        return $ either id (const b) z

-- | Run an action, turning exceptions which pass the predicate in to 'Nothing'
--   and re-throwing the rest.
--
--   Usage:
--   @
--   tryOpenFile :: MonadIO m => FilePath -> m (Maybe Handle)
--   tryOpenFile path =
--     liftIO . hushM isDoesNotExistError $
--       openBinaryFile path ReadMode
--   @
hushM :: (MonadCatch m, Exception e) => (e -> Bool) -> m a -> m (Maybe a)
hushM p m =
  let
    onError e =
      if p e then
        return Nothing
      else
        throwM e
  in
    handle onError $
      liftM Just m
