{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
module X.Data.Vector.Stream.Group (
    embed
  , concatWith
  , chunked
  , grouped
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

-- | Turns a stream of @a@ in to a stream of chunks of size @n@.
chunked :: (PrimMonad b, MonadBase b m, Generic.Vector v a) => Int -> Stream m a -> Stream m (v a)
chunked n xs =
  if n <= 0 then
    Savage.error "X.Data.Vector.Stream.Group.chunked: chunk size must be greater than zero"
  else
    grouped (\_ _ -> False) n xs
{-# INLINE [1] chunked #-}

-- | Turns a stream of @a@ in to a stream of chunks of at least size @n@,
--   except for the last one. Values of @a@ which are equal according to the
--   comparison function stay in the same chunk.
grouped :: (PrimMonad b, MonadBase b m, Generic.Vector v a) => (a -> a -> Bool) -> Int -> Stream m a -> Stream m (v a)
grouped eq n (Stream step t) =
  if n <= 0 then
    Savage.error "X.Data.Vector.Stream.Group.grouped: chunk size must be greater than zero"
  else
    embed $ do
      let
        notEq mx y =
          case mx of
            Nothing' ->
              True
            Just' x ->
              not (eq x y)
        {-# INLINE [0] notEq #-}

        loop = \case
          Nothing' ->
            pure Done

          Just' (s0, i, last, g0) ->
            step s0 >>= \case
              Yield x s ->
                if i >= n && last `notEq` x then do
                  xs <- liftBase $ Grow.unsafeFreeze g0
                  g <- liftBase $ Grow.new n
                  liftBase $ Grow.add g x
                  pure . Yield xs $ Just' (s, 1, Just' x, g)
                else do
                  liftBase $ Grow.add g0 x
                  pure . Skip $ Just' (s, i + 1, Just' x, g0)
              Skip s ->
                pure . Skip $ Just' (s, i, last, g0)
              Done ->
                if i == 0 then
                  pure $ Skip Nothing'
                else do
                  xs <- liftBase $ Grow.unsafeFreeze g0
                  pure $ Yield xs Nothing'
        {-# INLINE [0] loop #-}

      g <- liftBase $ Grow.new n

      pure .
        Stream loop $ Just' (t, 0, Nothing', g)
{-# INLINE [1] grouped #-}
