{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
module X.Data.Vector.Generic (
    module Generic

  -- * Accessors

  -- ** Length information

  , lengths

  -- * Elementwise operations

  -- ** Mapping
  , mapMaybe
  , imapMaybe

  -- ** Monadic mapping
  , mapMaybeM
  , imapMaybeM

  -- * Modifying vectors

  -- ** Transposition
  , transpose

  -- * Fusion support

  -- ** Conversion to/from bundles
  , unstreamM
  ) where

import           Control.Monad.ST (ST)

import           Data.Vector.Fusion.Bundle (Step(..))
import qualified Data.Vector.Fusion.Bundle as Bundle
import           Data.Vector.Fusion.Bundle.Monadic (Bundle(..))
import qualified Data.Vector.Fusion.Bundle.Monadic as MBundle
import           Data.Vector.Fusion.Bundle.Size (toMax)
import           Data.Vector.Fusion.Stream.Monadic (Stream(..))
import qualified Data.Vector.Fusion.Stream.Monadic as Stream

import           Data.Vector.Generic as Generic
import qualified Data.Vector.Generic.Mutable as MGeneric
import qualified Data.Vector.Unboxed as Unboxed
import qualified Data.Vector.Unboxed.Mutable as MUnboxed

import           P hiding (for, mapMaybe)


-- | The 'transpose' function transposes the rows and columns of its argument.
--
--   For example:
--
--   > transpose [[1,2,3],[4,5,6]] == [[1,4],[2,5],[3,6]]
--
--   If some of the rows are shorter than the following rows, their elements are skipped:
--
--   > transpose [[10,11],[20],[],[30,31,32]] == [[10,20,30],[11,31],[32]]
--
transpose :: (Vector va a, Vector vv (va a)) => vv (va a) -> vv (va a)
transpose xss =
  if Generic.null xss then
    Generic.empty
  else
    let
      MinMax min_cols max_cols =
        Unboxed.foldl' minmax (MinMax maxBound minBound) (lengths xss)
    in
      if min_cols == max_cols then
        transposeMatrix xss max_cols
      else
        transposeJagged xss max_cols
{-# INLINE transpose #-}

transposeMatrix :: forall vv va a. (Vector va a, Vector vv (va a)) => vv (va a) -> Int -> vv (va a)
transposeMatrix xss n_cols =
  let
    n_rows :: Int
    !n_rows =
      Generic.length xss
  in
    Generic.create $ do
      yss <- MGeneric.unsafeNew n_cols
      ys0 <- MGeneric.unsafeNew (n_cols * n_rows)

      -- Walk rows in blocks of 16, so that they're kept in cache between loops
      -- over columns - benchmarking suggests that blocks of 16 is a reasonably
      -- optimal choice.
      for' 16 0 n_rows $ \b ->
        for 0 n_cols $ \i ->
          for b (min (b + 16) n_rows) $ \j ->
            let
              !xs = xss `Generic.unsafeIndex` j
              !x  = xs  `Generic.unsafeIndex` i
            in
              MGeneric.unsafeWrite ys0 (i * n_rows + j) x

      for 0 n_cols $ \i -> do
        !ys <-
          Generic.unsafeFreeze $
          MGeneric.unsafeSlice (i * n_rows) n_rows ys0

        MGeneric.unsafeWrite yss i ys

      pure yss
{-# INLINE transposeMatrix #-}

transposeJagged :: forall vv va a. (Vector va a, Vector vv (va a)) => vv (va a) -> Int -> vv (va a)
transposeJagged xss max_cols =
  let
    n_rows :: Int
    !n_rows =
      Generic.length xss
  in
    Generic.create $ do
      yss <- MGeneric.unsafeNew max_cols
      ys0 <- MGeneric.unsafeNew (max_cols * n_rows)
      ns <- MUnboxed.replicate max_cols 0

      -- Walk rows in blocks of 16, so that they're kept in cache between loops
      -- over columns - benchmarking suggests that blocks of 16 is a reasonably
      -- optimal choice.
      for' 16 0 n_rows $ \b ->
        for 0 max_cols $ \i ->
          for b (min (b + 16) n_rows) $ \j ->
            let
              !xs = xss `Generic.unsafeIndex` j
            in
              case i < Generic.length xs of
                False ->
                  pure ()
                True -> do
                  let
                    !x = xs `Generic.unsafeIndex` i
                  !n <- MGeneric.unsafeRead ns i
                  MGeneric.unsafeWrite ys0 (i * n_rows + n) x
                  MGeneric.unsafeWrite ns i (n + 1)

      for 0 max_cols $ \i -> do
        !n <-
          MGeneric.unsafeRead ns i

        !ys <-
          Generic.unsafeFreeze $
          MGeneric.unsafeSlice (i * n_rows) n ys0

        MGeneric.unsafeWrite yss i ys

      pure yss
{-# INLINE transposeJagged #-}


lengths :: (Vector va a, Vector vv (va a), Vector vn Int) => vv (va a) -> vn Int
lengths =
  unstream .
  Bundle.map Generic.length .
  Bundle.reVector .
  stream
{-# INLINE lengths #-}

mapMaybe :: (Vector v a, Vector v b) => (a -> Maybe b) -> v a -> v b
mapMaybe f =
  unstream .
  Bundle.inplace (mapMaybeStream (return . f)) toMax .
  stream
{-# INLINE mapMaybe #-}

imapMaybe :: (Vector v a, Vector v b) => (Int -> a -> Maybe b) -> v a -> v b
imapMaybe f =
  unstream .
  Bundle.inplace (mapMaybeStream (return . uncurry f) . Stream.indexed) toMax .
  stream
{-# INLINE imapMaybe #-}

mapMaybeM :: Monad m => (Vector v a, Vector v b) => (a -> m (Maybe b)) -> v a -> m (v b)
mapMaybeM f =
  unstreamM .
  mapMaybeBundle f .
  Bundle.lift .
  stream
{-# INLINE mapMaybeM #-}

imapMaybeM :: Monad m => (Vector v a, Vector v b) => (Int -> a -> m (Maybe b)) -> v a -> m (v b)
imapMaybeM f =
  unstreamM .
  imapMaybeBundle f .
  Bundle.lift .
  stream
{-# INLINE imapMaybeM #-}

mapMaybeBundle :: Monad m => (a -> m (Maybe b)) -> Bundle m v a -> Bundle m v b
mapMaybeBundle f Bundle {sElems = s, sSize = n} =
  MBundle.fromStream (mapMaybeStream f s) (toMax n)
{-# INLINE mapMaybeBundle #-}

imapMaybeBundle :: Monad m => (Int -> a -> m (Maybe b)) -> Bundle m v a -> Bundle m v b
imapMaybeBundle f Bundle {sElems = s, sSize = n} =
  MBundle.fromStream (mapMaybeStream (uncurry f) $ Stream.indexed s) (toMax n)
{-# INLINE imapMaybeBundle #-}

-- include/vector.h
#define INLINE_FUSED INLINE [1]
#define INLINE_INNER INLINE [0]

mapMaybeStream :: Monad m => (a -> m (Maybe b)) -> Stream m a -> Stream m b
mapMaybeStream f (Stream step t) =
  let
    step' s = do
      r <- step s
      case r of
        Yield x s' -> do
          mb <- f x
          return $
            case mb of
              Nothing ->
                Skip s'
              Just y ->
                Yield y s'
        Skip s' ->
          return $ Skip s'
        Done ->
          return $ Done
    {-# INLINE_INNER step' #-}
  in
    Stream step' t
{-# INLINE_FUSED mapMaybeStream #-}

-- Not exported by Data.Vector.Generic
unstreamM :: (Monad m, Vector v a) => Bundle m u a -> m (v a)
unstreamM s = do
  xs <- MBundle.toList s
  return $ unstream $ Bundle.unsafeFromList (MBundle.size s) xs
{-# INLINE_FUSED unstreamM #-}

------------------------------------------------------------------------
-- Utils

for :: Int -> Int -> (Int -> ST s ()) -> ST s ()
for !n0 !n f =
  let
    loop !i =
      case i == n of
        True ->
          return ()
        False ->
          f i >> loop (i+1)
  in
    loop n0
{-# INLINE for #-}

for' :: Int -> Int -> Int -> (Int -> ST s ()) -> ST s ()
for' !inc !n0 !n f =
  let
    loop !i =
      case i >= n of
        True ->
          return ()
        False ->
          f i >> loop (i+inc)
  in
    loop n0
{-# INLINE for' #-}

data MinMax =
  MinMax !Int !Int

minmax :: MinMax -> Int -> MinMax
minmax (MinMax min0 max0) n =
  MinMax (min min0 n) (max max0 n)
{-# INLINE minmax #-}
