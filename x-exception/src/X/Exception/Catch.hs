{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module X.Exception.Catch (
    bracketF
  , unsafeBracket
  , unsafeBracket_
  , unsafeFinally
  ) where

import           Control.Applicative (pure, (<$>))
import           Control.Monad (return)
import           Control.Monad.Catch hiding (finally)

import           Data.Either
import           Data.Function

import           System.IO


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
bracketF :: IO a -> (a -> IO (Either b c)) -> (a -> IO b) -> IO b
bracketF a f g =
  mask $ \restore -> do
    a' <- a
    x <- restore (BracketOk <$> g a') `catchAll`
           (\ex -> either BracketFailedFinalizerError (const $ BracketFailedFinalizerOk ex) <$> f a')
    case x of
      BracketFailedFinalizerOk ex ->
        throwM ex
      BracketFailedFinalizerError b ->
        pure b
      BracketOk b -> do
        z <- f a'
        pure $ either id (const b) z

-- This is a bracket which can be used with monad transformers such as `EitherT` where
-- you are not concerned with asyncronus errors
unsafeBracket :: MonadCatch m => m a -> (a -> m b) -> (a -> m c) -> m c
unsafeBracket resource finalizer action = do
  a <- resource
  r' <- action a `onException` finalizer a
  _ <- finalizer a
  return r'

unsafeBracket_ :: MonadCatch m => m a -> m b -> m c -> m c
unsafeBracket_ before after action =
  unsafeBracket before (const after) (const action)

unsafeFinally :: MonadCatch m => m a -> m b -> m a
unsafeFinally action finalizer =
  unsafeBracket_ (return ()) finalizer action
