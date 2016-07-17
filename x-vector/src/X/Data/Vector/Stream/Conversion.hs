{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
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
import qualified Data.Vector.Generic.New           as VGN
import qualified Data.Vector.Fusion.Stream.Monadic as VS
import qualified Data.Vector.Fusion.Bundle.Monadic as VB
import qualified Data.Vector.Fusion.Bundle.Size    as VBS
import qualified Data.Vector.Fusion.Bundle as Bundle
import qualified Data.Vector.Fusion.Bundle.Monadic as MBundle

import Data.Vector.Fusion.Util (Id(..))

import           P


vectorOfStream :: VG.Vector v a => VS.Stream Id a -> v a
vectorOfStream !strm
 = let !bundle = VB.fromStream strm VBS.Unknown
   in  VG.unstream bundle
-- Delay inlining so the rule has a chance to fire
{-# INLINE [0] vectorOfStream #-}
-- Short-cut fusion: converting a stream to a vector and back is identity (roughly)
{-# RULES
    "stream/vectorOfStream [X-Vector]" forall s.
        VG.stream (vectorOfStream s) = VB.fromStream s VBS.Unknown
 #-}


streamOfVector :: VG.Vector v a => v a -> VS.Stream Id a
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
         => (VS.Stream Id a -> VS.Stream Id b)
         -> (VBS.Size -> VBS.Size)
         -> v a -> u b
inplace stream size vec
 = VG.unstream (inplaceDelay stream size vec)
-- Inline the 'unstream' part as soon as possible, because these are used in rewrite rules.
-- However, we need to delay inlining the rest of it, because otherwise the function calls
-- get converted to lets and these get in the way of rewrite rules.
--
-- > inplace stream size vec
-- >  = unstream (fromStream (stream (streamOfVector vec)) (size (Exact (length vec))))
-- >
-- > use_place = stream (inplace id id input_vec)
--
-- =(INLINE inplace)>
--
-- > use_place
-- >  = stream
-- >  ( let a = id (streamOfVector input_vec)
-- >        b = id (Exact (length input_vec))
-- >        c = fromStream a b
-- >     in unstream c )
--
-- Here, we end up with these awful lets, so "stream (unstream ?)" cannot fire.
-- By delaying inplaceDelay, the lets are not generated until later, and so stay out of the way.
-- I do not know why these lets show up.
{-# INLINE inplace #-}

inplaceDelay :: VG.Vector v a => VG.Vector u b
         => (VS.Stream Id a -> VS.Stream Id b)
         -> (VBS.Size -> VBS.Size)
         -> v a
         -> VB.Bundle Id u b
inplaceDelay stream size vec
 = let stream' = stream (streamOfVector vec)
       size'   = size (VBS.Exact $ VG.length vec)
   in VB.fromStream stream' size'
{-# INLINE [0] inplaceDelay #-}
{-# RULES

"inplaceDelay/inplaceDelay [X-Vector]"
  forall f1 f2
         g1 g2 s.
  inplaceDelay f1 g1 (VG.unstream (inplaceDelay f2 g2 s)) = inplaceDelay (f1 . f2) (g1 . g2) s

  -- Depending on when VG.unstream decides to inline, we might need to deal with its unfolding.
  -- This was necessary for GHC-7.8, with same version of vector.
  -- Perhaps newer versions interleave rewriting with inlining.
"inplaceDelay/new/inplaceDelay [X-Vector]"
  forall f1 f2
         g1 g2 s.
  inplaceDelay f1 g1 (VG.new (VGN.unstream (inplaceDelay f2 g2 s))) = inplaceDelay (f1 . f2) (g1 . g2) s

  #-}




listOfStream :: VS.Stream Id a -> [a]
listOfStream = unId . listOfStreamM
{-# INLINE listOfStream #-}

streamOfList :: [a] -> VS.Stream Id a
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

