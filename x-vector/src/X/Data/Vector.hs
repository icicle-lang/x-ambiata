{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
module X.Data.Vector (
    module Boxed

  -- * Elementwise operations

  -- ** Mapping
  , mapMaybe
  , imapMaybe

  -- ** Monadic mapping
  , mapMaybeM
  , imapMaybeM
  ) where

import           Data.Vector as Boxed

import           P hiding (mapMaybe)

import qualified X.Data.Vector.Generic as Generic


mapMaybe :: (a -> Maybe b) -> Vector a -> Vector b
mapMaybe =
  Generic.mapMaybe
{-# INLINE mapMaybe #-}

imapMaybe :: (Int -> a -> Maybe b) -> Vector a -> Vector b
imapMaybe =
  Generic.imapMaybe
{-# INLINE imapMaybe #-}

mapMaybeM :: Monad m => (a -> m (Maybe b)) -> Vector a -> m (Vector b)
mapMaybeM =
  Generic.mapMaybeM
{-# INLINE mapMaybeM #-}

imapMaybeM :: Monad m => (Int -> a -> m (Maybe b)) -> Vector a -> m (Vector b)
imapMaybeM =
  Generic.imapMaybeM
{-# INLINE imapMaybeM #-}
