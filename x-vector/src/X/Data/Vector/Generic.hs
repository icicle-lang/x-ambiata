{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
module X.Data.Vector.Generic (
    module Generic

  -- * Elementwise operations

  -- ** Mapping
  , mapMaybe
  , imapMaybe

  -- ** Monadic mapping
  , mapMaybeM
  , imapMaybeM

  -- * Fusion support

  -- ** Conversion to/from bundles
  , unstreamM
  ) where

import           Data.Vector.Fusion.Bundle (Step(..))
import qualified Data.Vector.Fusion.Bundle as Bundle
import           Data.Vector.Fusion.Bundle.Monadic (Bundle(..))
import qualified Data.Vector.Fusion.Bundle.Monadic as MBundle
import           Data.Vector.Fusion.Bundle.Size (toMax)
import           Data.Vector.Fusion.Stream.Monadic (Stream(..))
import qualified Data.Vector.Fusion.Stream.Monadic as Stream

import           Data.Vector.Generic as Generic

import           P hiding (mapMaybe)


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
