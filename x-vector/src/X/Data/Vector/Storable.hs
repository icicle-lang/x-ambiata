{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
module X.Data.Vector.Storable (
    module Storable

  -- * Elementwise operations

  -- ** Mapping
  , mapMaybe
  , imapMaybe

  -- ** Monadic mapping
  , mapMaybeM
  , imapMaybeM
  ) where

import           Data.Vector.Storable as Storable

import           P hiding (mapMaybe)

import qualified X.Data.Vector.Generic as Generic


mapMaybe :: (Storable a, Storable b) => (a -> Maybe b) -> Vector a -> Vector b
mapMaybe =
  Generic.mapMaybe
{-# INLINE mapMaybe #-}

imapMaybe :: (Storable a, Storable b) => (Int -> a -> Maybe b) -> Vector a -> Vector b
imapMaybe =
  Generic.imapMaybe
{-# INLINE imapMaybe #-}

mapMaybeM :: (Monad m, Storable a, Storable b) => (a -> m (Maybe b)) -> Vector a -> m (Vector b)
mapMaybeM =
  Generic.mapMaybeM
{-# INLINE mapMaybeM #-}

imapMaybeM :: (Monad m, Storable a, Storable b) => (Int -> a -> m (Maybe b)) -> Vector a -> m (Vector b)
imapMaybeM =
  Generic.imapMaybeM
{-# INLINE imapMaybeM #-}
