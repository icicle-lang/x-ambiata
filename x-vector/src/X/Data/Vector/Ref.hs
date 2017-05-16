{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
module X.Data.Vector.Ref (
    Ref
  , newRef
  , readRef
  , writeRef
  , modifyRef
  , modifyRefM

  -- * Convenience Specialisations
  , newBoxedRef
  , newUnboxedRef
  , newStorableRef
  ) where

import           Control.Monad.Primitive (PrimMonad, PrimState)

import           Data.Vector.Generic.Mutable (MVector)
import qualified Data.Vector.Generic.Mutable as MGeneric
import qualified Data.Vector.Mutable as MBoxed
import qualified Data.Vector.Storable.Mutable as MStorable
import           Data.Vector.Unboxed (Unbox)
import qualified Data.Vector.Unboxed.Mutable as MUnboxed

import           Foreign.Storable (Storable)

import           P


newtype Ref v s a =
  Ref {
      _unRef :: v s a
    }

newRef :: (PrimMonad m, MVector v a) => a -> m (Ref v (PrimState m) a)
newRef =
  liftM Ref . MGeneric.replicate 1
{-# INLINE newRef #-}

newBoxedRef :: PrimMonad m => a -> m (Ref MBoxed.MVector (PrimState m) a)
newBoxedRef =
  newRef
{-# INLINE newBoxedRef #-}

newUnboxedRef :: (PrimMonad m, Unbox a) => a -> m (Ref MUnboxed.MVector (PrimState m) a)
newUnboxedRef =
  newRef
{-# INLINE newUnboxedRef #-}

newStorableRef :: (PrimMonad m, Storable a) => a -> m (Ref MStorable.MVector (PrimState m) a)
newStorableRef =
  newRef
{-# INLINE newStorableRef #-}

readRef :: (PrimMonad m, MVector v a) => Ref v (PrimState m) a -> m a
readRef (Ref ref) =
  MGeneric.unsafeRead ref 0
{-# INLINE readRef #-}

writeRef :: (PrimMonad m, MVector v a) => Ref v (PrimState m) a -> a -> m ()
writeRef (Ref ref) =
  MGeneric.unsafeWrite ref 0
{-# INLINE writeRef #-}

modifyRef :: (PrimMonad m, MVector v a) => Ref v (PrimState m) a -> (a -> a) -> m ()
modifyRef (Ref ref) f = do
  x <- MGeneric.unsafeRead ref 0
  MGeneric.unsafeWrite ref 0 $ f x
{-# INLINE modifyRef #-}

modifyRefM :: (PrimMonad m, MVector v a) => Ref v (PrimState m) a -> (a -> m a) -> m ()
modifyRefM (Ref ref) f = do
  x0 <- MGeneric.unsafeRead ref 0
  x <- f x0
  MGeneric.unsafeWrite ref 0 x
{-# INLINE modifyRefM #-}
