{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP #-}
module X.Data.Vector.Unboxed (
    module Unboxed

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
import           Data.Vector.Unboxed as Unboxed

import           P hiding (mapMaybe)

import qualified X.Data.Vector.Generic as Generic


transpose :: Unbox a => Boxed.Vector (Vector a) -> Boxed.Vector (Vector a)
transpose =
  Generic.transpose
{-# INLINE transpose #-}

#if !MIN_VERSION_vector(0,12,0)
mapMaybe :: (Unbox a, Unbox b) => (a -> Maybe b) -> Vector a -> Vector b
mapMaybe =
  Generic.mapMaybe
{-# INLINE mapMaybe #-}

imapMaybe :: (Unbox a, Unbox b) => (Int -> a -> Maybe b) -> Vector a -> Vector b
imapMaybe =
  Generic.imapMaybe
{-# INLINE imapMaybe #-}
#endif

mapMaybeM :: (Monad m, Unbox a, Unbox b) => (a -> m (Maybe b)) -> Vector a -> m (Vector b)
mapMaybeM =
  Generic.mapMaybeM
{-# INLINE mapMaybeM #-}

imapMaybeM :: (Monad m, Unbox a, Unbox b) => (Int -> a -> m (Maybe b)) -> Vector a -> m (Vector b)
imapMaybeM =
  Generic.imapMaybeM
{-# INLINE imapMaybeM #-}

mapAccumulate :: (Unbox elt, Unbox elt') => (acc -> elt -> (acc, elt')) -> acc -> Vector elt -> Vector elt'
mapAccumulate =
  Generic.mapAccumulate
{-# INLINE mapAccumulate #-}

mapAccumulateM :: (Monad m, Unbox elt, Unbox elt') => (acc -> elt -> m (acc, elt')) -> acc -> Vector elt -> m (Vector elt')
mapAccumulateM =
  Generic.mapAccumulateM
{-# INLINE mapAccumulateM #-}

merge :: Unbox a => (a -> a -> Generic.MergePullFrom a) -> Vector a -> Vector a -> Vector a
merge =
  Generic.merge
{-# INLINE merge #-}

uncons :: Unbox a => Vector a -> Maybe (a, Vector a)
uncons =
  Generic.uncons
{-# INLINE uncons #-}
