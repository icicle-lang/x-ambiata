{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE LambdaCase #-}
module X.Data.Vector.Stream.Conversion
  ( vectorOfStream
  , streamOfVector
  , vectorOfStreamM
  , streamOfVectorM
  , inplaceM
  , inplace

  , listOfStream
  , streamOfList
  , listOfStreamM
  , streamOfListM

  , vectorOfBundleM
  ) where

import qualified Data.Vector.Generic               as VG
import qualified Data.Vector.Fusion.Stream.Monadic as VS
import qualified Data.Vector.Fusion.Bundle.Monadic as VB
import qualified Data.Vector.Fusion.Bundle.Size    as VBS
import qualified Data.Vector.Fusion.Bundle as Bundle
import qualified Data.Vector.Fusion.Bundle.Monadic as MBundle

import Data.Functor.Identity (runIdentity, Identity)
import Data.Vector.Fusion.Util (Id(..))

import           P

vectorOfStream :: VG.Vector v a => VS.Stream Identity a -> v a
vectorOfStream = runIdentity . vectorOfStreamM
{-# INLINE vectorOfStream #-}


streamOfVector :: VG.Vector v a => v a -> VS.Stream Identity a
streamOfVector = streamOfVectorM
{-# INLINE streamOfVector #-}


vectorOfStreamM :: Monad m => VG.Vector v a => VS.Stream m a -> m (v a)
vectorOfStreamM strm
 = vectorOfBundleM
 $ VB.fromStream strm VBS.Unknown
{-# INLINE vectorOfStreamM #-}

streamOfVectorM :: Monad m => VG.Vector v a => v a -> VS.Stream m a
streamOfVectorM vec
 = VB.elements
 $ VB.fromVector vec
{-# INLINE streamOfVectorM #-}


inplaceM :: Monad m => VG.Vector v a => VG.Vector u b
         => (VS.Stream m a -> VS.Stream m b)
         -> (VBS.Size -> VBS.Size)
         -> v a -> m (u b)
inplaceM stream size vec
 = let stream' = stream (streamOfVectorM vec)
       size'   = size (VBS.Exact $ VG.length vec)
   in vectorOfBundleM (VB.fromStream stream' size')
{-# INLINE inplaceM #-}

inplace  :: VG.Vector v a => VG.Vector u b
         => (VS.Stream Identity a -> VS.Stream Identity b)
         -> (VBS.Size -> VBS.Size)
         -> v a -> u b
inplace stream size vec
 = runIdentity $ inplaceM stream size vec
{-# INLINE inplace #-}




listOfStream :: VS.Stream Identity a -> [a]
listOfStream = runIdentity . listOfStreamM
{-# INLINE listOfStream #-}

streamOfList :: [a] -> VS.Stream Identity a
streamOfList = streamOfListM
{-# INLINE streamOfList #-}


listOfStreamM :: Monad m => VS.Stream m a -> m [a]
listOfStreamM = VS.toList
{-# INLINE listOfStreamM #-}

streamOfListM :: Monad m => [a] -> VS.Stream m a
streamOfListM = VS.fromList
{-# INLINE streamOfListM #-}



-- | Called @unstreamM@ in Data.Vector.Generic, but not exported
vectorOfBundleM :: (Monad m, VG.Vector v a) => MBundle.Bundle m v a -> m (v a)
vectorOfBundleM s = do
  xs <- MBundle.toList s
  return $ VG.unstream $ Bundle.unsafeFromList (MBundle.size s) xs
{-# INLINE [1] vectorOfBundleM #-}

