{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
module X.Data.Vector.Primitive (
    module Primitive

  -- * Elementwise operations

  -- ** Mapping
  , mapMaybe
  , imapMaybe

  -- ** Monadic mapping
  , mapMaybeM
  , imapMaybeM
  ) where

import           Data.Vector.Primitive as Primitive

import           P hiding (mapMaybe)

import qualified X.Data.Vector.Generic as Generic


mapMaybe :: (Prim a, Prim b) => (a -> Maybe b) -> Vector a -> Vector b
mapMaybe =
  Generic.mapMaybe
{-# INLINE mapMaybe #-}

imapMaybe :: (Prim a, Prim b) => (Int -> a -> Maybe b) -> Vector a -> Vector b
imapMaybe =
  Generic.imapMaybe
{-# INLINE imapMaybe #-}

mapMaybeM :: (Monad m, Prim a, Prim b) => (a -> m (Maybe b)) -> Vector a -> m (Vector b)
mapMaybeM =
  Generic.mapMaybeM
{-# INLINE mapMaybeM #-}

imapMaybeM :: (Monad m, Prim a, Prim b) => (Int -> a -> m (Maybe b)) -> Vector a -> m (Vector b)
imapMaybeM =
  Generic.imapMaybeM
{-# INLINE imapMaybeM #-}
