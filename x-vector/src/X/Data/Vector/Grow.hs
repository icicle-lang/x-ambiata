{-# LANGUAGE NoImplicitPrelude #-}
module X.Data.Vector.Grow (
    Grow
  , new
  , length
  , capacity
  , add
  , append
  , clear
  , freeze
  , unsafeFreeze
  , unsafeElems
  ) where

import           X.Data.Vector.Ref (Ref, newRef, readRef, modifyRef, modifyRefM)

import           Control.Monad.Primitive (PrimMonad, PrimState)

import           Data.Vector.Generic (Vector, Mutable)
import qualified Data.Vector.Generic as Generic
import           Data.Vector.Generic.Mutable (MVector)
import qualified Data.Vector.Generic.Mutable as MGeneric
import qualified Data.Vector.Mutable as MBoxed

import           P hiding (length)


data GrowState v s a =
  GrowState {
      growLength :: !Int
    , growVector :: !(v s a)
    }

newtype Grow v s a =
  Grow {
      unGrow :: Ref MBoxed.MVector s (GrowState v s a)
    }

new :: (PrimMonad m, MVector v a) => Int -> m (Grow v (PrimState m) a)
new n = do
  gs <- liftM (GrowState 0) $ MGeneric.new n
  liftM Grow $ newRef gs
{-# INLINE new #-}

length :: PrimMonad m => Grow v (PrimState m) a -> m Int
length =
  liftM growLength . readRef . unGrow
{-# INLINE length #-}

capacity :: (PrimMonad m, MVector v a) => Grow v (PrimState m) a -> m Int
capacity =
  liftM (MGeneric.length . growVector) . readRef . unGrow
{-# INLINE capacity #-}

expand :: (PrimMonad m, MVector v a) => GrowState v (PrimState m) a -> Int -> m (GrowState v (PrimState m) a)
expand gs@(GrowState n xs0) m =
  if n + m > MGeneric.length xs0 then
    liftM (GrowState n) $
      -- Grow *by* `max n m` so the new length is `max (2 * n) (n + m)`.
      MGeneric.unsafeGrow xs0 (max n m)
  else
    return gs
{-# INLINE expand #-}

add :: (PrimMonad m, MVector v a) => Grow v (PrimState m) a -> a -> m ()
add (Grow ref) x =
 modifyRefM ref $ \gs0 -> do
   GrowState n xs <- expand gs0 1
   MGeneric.unsafeWrite xs n x
   return $ GrowState (n + 1) xs
{-# INLINE add #-}

append :: (PrimMonad m, Vector v a) => Grow (Mutable v) (PrimState m) a -> v a -> m ()
append (Grow ref) ys =
 modifyRefM ref $ \gs0 -> do
   let
     m =
       Generic.length ys
   GrowState n xs <- expand gs0 m
   Generic.unsafeCopy (MGeneric.unsafeSlice n m xs) ys
   return $ GrowState (n + m) xs
{-# INLINE append #-}

clear :: PrimMonad m => Grow v (PrimState m) a -> m ()
clear (Grow ref) =
 modifyRef ref $ \(GrowState _ xs) ->
   GrowState 0 xs
{-# INLINE clear #-}

freeze :: (PrimMonad m, Vector v a) => Grow (Mutable v) (PrimState m) a -> m (v a)
freeze =
  bind Generic.freeze . unsafeElems
{-# INLINE freeze #-}

unsafeFreeze :: (PrimMonad m, Vector v a) => Grow (Mutable v) (PrimState m) a -> m (v a)
unsafeFreeze =
  bind Generic.unsafeFreeze . unsafeElems
{-# INLINE unsafeFreeze #-}

unsafeElems :: (PrimMonad m, MVector v a) => Grow v (PrimState m) a -> m (v (PrimState m) a)
unsafeElems (Grow ref) = do
  GrowState n xs <- readRef ref
  return $ MGeneric.unsafeTake n xs
{-# INLINE unsafeElems #-}
