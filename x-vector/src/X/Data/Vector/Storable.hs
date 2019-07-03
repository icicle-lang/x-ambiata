{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP #-}
module X.Data.Vector.Storable (
    module Storable

  -- * Accessors

  -- ** Destructors
  , uncons

  -- * Elementwise operations

  -- ** Mapping
#if !MIN_VERSION_vector(0,12,0)
  , mapMaybe
  , imapMaybe
#endif
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

import qualified Data.Vector as Boxed
import           Data.Vector.Storable as Storable

import           P hiding (mapMaybe)

import qualified X.Data.Vector.Generic as Generic


transpose :: Storable a => Boxed.Vector (Vector a) -> Boxed.Vector (Vector a)
transpose =
  Generic.transpose
{-# INLINE transpose #-}

#if !MIN_VERSION_vector(0,12,0)
mapMaybe :: (Storable a, Storable b) => (a -> Maybe b) -> Vector a -> Vector b
mapMaybe =
  Generic.mapMaybe
{-# INLINE mapMaybe #-}

imapMaybe :: (Storable a, Storable b) => (Int -> a -> Maybe b) -> Vector a -> Vector b
imapMaybe =
  Generic.imapMaybe
{-# INLINE imapMaybe #-}
#endif

mapMaybeM :: (Monad m, Storable a, Storable b) => (a -> m (Maybe b)) -> Vector a -> m (Vector b)
mapMaybeM =
  Generic.mapMaybeM
{-# INLINE mapMaybeM #-}

imapMaybeM :: (Monad m, Storable a, Storable b) => (Int -> a -> m (Maybe b)) -> Vector a -> m (Vector b)
imapMaybeM =
  Generic.imapMaybeM
{-# INLINE imapMaybeM #-}

mapAccumulate :: (Storable elt, Storable elt') => (acc -> elt -> (acc, elt')) -> acc -> Vector elt -> Vector elt'
mapAccumulate =
  Generic.mapAccumulate
{-# INLINE mapAccumulate #-}

mapAccumulateM :: (Monad m, Storable elt, Storable elt') => (acc -> elt -> m (acc, elt')) -> acc -> Vector elt -> m (Vector elt')
mapAccumulateM =
  Generic.mapAccumulateM
{-# INLINE mapAccumulateM #-}

merge :: Storable a => (a -> a -> Generic.MergePullFrom a) -> Vector a -> Vector a -> Vector a
merge =
  Generic.merge
{-# INLINE merge #-}

uncons :: Storable a => Vector a -> Maybe (a, Vector a)
uncons =
  Generic.uncons
{-# INLINE uncons #-}
