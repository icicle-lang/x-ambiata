{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
module X.Data.Vector (
    module Boxed

  -- * Accessors

  -- ** Destructors
  , uncons

  -- * Elementwise operations

  -- ** Mapping
  , mapMaybe
  , imapMaybe
  , mapAccumulate

  -- ** Monadic mapping
  , mapMaybeM
  , imapMaybeM
  , mapAccumulateM

  -- * Modifying vectors

  -- ** Transposition
  , transpose

  -- ** Merging
  , merge
  ) where

import           Data.Vector as Boxed

import           P hiding (mapMaybe)

import qualified X.Data.Vector.Generic as Generic


transpose :: Vector (Vector a) -> Vector (Vector a)
transpose =
  Generic.transpose
{-# INLINE transpose #-}

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

mapAccumulate :: (acc -> elt -> (acc, elt')) -> acc -> Vector elt -> Vector elt'
mapAccumulate =
  Generic.mapAccumulate
{-# INLINE mapAccumulate #-}

mapAccumulateM :: Monad m => (acc -> elt -> m (acc, elt')) -> acc -> Vector elt -> m (Vector elt')
mapAccumulateM =
  Generic.mapAccumulateM
{-# INLINE mapAccumulateM #-}

merge :: (a -> a -> Generic.MergePullFrom a) -> Vector a -> Vector a -> Vector a
merge =
  Generic.merge
{-# INLINE merge #-}

uncons :: Vector a -> Maybe (a, Vector a)
uncons =
  Generic.uncons
