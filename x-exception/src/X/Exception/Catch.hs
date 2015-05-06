{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module X.Exception.Catch (
    unsafeBracket
  , unsafeBracket_
  , unsafeFinally
  ) where

import           Control.Monad (return)
import           Control.Monad.Catch hiding (finally)

import           Data.Function

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
