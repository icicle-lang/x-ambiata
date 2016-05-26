{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
module X.Data.Vector.Unboxed (
    module Unboxed

  -- * Elementwise operations

  -- ** Mapping
  , mapMaybe
  , imapMaybe

  -- ** Monadic mapping
  , mapMaybeM
  , imapMaybeM
  ) where

import           Data.Vector.Unboxed as Unboxed

import           P hiding (mapMaybe)

import qualified X.Data.Vector.Generic as Generic


mapMaybe :: (Unbox a, Unbox b) => (a -> Maybe b) -> Vector a -> Vector b
mapMaybe =
  Generic.mapMaybe
{-# INLINE mapMaybe #-}

imapMaybe :: (Unbox a, Unbox b) => (Int -> a -> Maybe b) -> Vector a -> Vector b
imapMaybe =
  Generic.imapMaybe
{-# INLINE imapMaybe #-}

mapMaybeM :: (Monad m, Unbox a, Unbox b) => (a -> m (Maybe b)) -> Vector a -> m (Vector b)
mapMaybeM =
  Generic.mapMaybeM
{-# INLINE mapMaybeM #-}

imapMaybeM :: (Monad m, Unbox a, Unbox b) => (Int -> a -> m (Maybe b)) -> Vector a -> m (Vector b)
imapMaybeM =
  Generic.imapMaybeM
{-# INLINE imapMaybeM #-}
