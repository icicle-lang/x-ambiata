{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
module X.Data.Vector.Stream.Group (
    embed
  , concatWith
  , chunksOf
  ) where

import           Control.Monad.Base (MonadBase(..))
import           Control.Monad.Primitive (PrimMonad(..))

import           Data.Vector.Fusion.Stream.Monadic (Stream(..), Step(..))
import qualified Data.Vector.Fusion.Stream.Monadic as Stream
import qualified Data.Vector.Generic as Generic

import           P

import qualified Prelude as Savage

import qualified X.Data.Vector.Grow as Grow


embed :: Monad m => m (Stream m a) -> Stream m a
embed mstream =
  Stream.concatMapM (const mstream) (Stream.singleton ())
{-# INLINE [1] embed #-}

concatWith :: Monad m => Stream m a -> (a -> Stream m b) -> Stream m b
concatWith =
  flip Stream.concatMap
{-# INLINE [1] concatWith #-}

chunksOf :: (PrimMonad b, MonadBase b m, Generic.Vector v a) => Int -> Stream m a -> Stream m (v a)
chunksOf !n (Stream step t) =
  if n <= 0 then
    Savage.error "X.Data.Vector.Stream.Group.chunksOf: chunk size must be greater than zero"
  else
    embed $ do
      let
        loop = \case
          Nothing' ->
            pure Done

          Just' (s0, i, g0) ->
            if i == n then do
              xs <- liftBase $ Grow.unsafeFreeze g0
              g <- liftBase $ Grow.new n
              pure . Yield xs $ Just' (s0, 0, g)
            else
              step s0 >>= \case
                Yield x s -> do
                  liftBase $ Grow.add g0 x
                  pure . Skip $ Just' (s, i + 1, g0)
                Skip s ->
                  pure . Skip $ Just' (s, i, g0)
                Done ->
                  if i == 0 then
                    pure $ Skip Nothing'
                  else do
                    xs <- liftBase $ Grow.unsafeFreeze g0
                    pure $ Yield xs Nothing'
        {-# INLINE [0] loop #-}

      g <- liftBase $ Grow.new n

      pure .
        Stream loop $ Just' (t, 0, g)
{-# INLINE [1] chunksOf #-}
