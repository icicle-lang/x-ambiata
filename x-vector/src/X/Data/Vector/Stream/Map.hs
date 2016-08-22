{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE LambdaCase #-}
module X.Data.Vector.Stream.Map
  ( mapAccumulateM
  , mapAccumulate
  , mapMaybeM
  , mapMaybe
  , imapMaybeM
  , imapMaybe
  ) where

import qualified Data.Vector.Fusion.Stream.Monadic as VS

import Data.Vector.Fusion.Util (Id(..))

import           P hiding (mapMaybe)


-- | This is very similar to @postscanl@ etc, but the accumulator and element types are completely separate.
-- @postscanl@ could be implemented in terms of this fairly easily, but implementing this in terms of @postscanl@
-- would require an @fmap (fromJust . snd)@ afterwards and having the scan accumulator of type @(acc, Maybe elt')@ or something.
mapAccumulateM :: Monad m => (acc -> elt -> m (acc, elt')) -> acc -> VS.Stream m elt -> VS.Stream m elt'
mapAccumulateM f z (VS.Stream step state)
 = VS.Stream go (state,z)
 where
  go (s,acc)
   = step s >>=
   \case
      VS.Yield v s'
       -> do (acc',v') <- f acc v
             return $ VS.Yield v' (s',acc')
      VS.Skip    s'
       ->    return $ VS.Skip     (s',acc)
      VS.Done
       ->    return $ VS.Done
  {-# INLINE [0] go #-}
{-# INLINE [1] mapAccumulateM #-}

mapAccumulate :: (acc -> elt -> (acc, elt')) -> acc -> VS.Stream Id elt -> VS.Stream Id elt'
mapAccumulate f z v
 = mapAccumulateM (\a b -> return $ f a b) z v
{-# INLINE [1] mapAccumulate #-}


mapMaybeM :: Monad m => (a -> m (Maybe b)) -> VS.Stream m a -> VS.Stream m b
mapMaybeM f (VS.Stream step t) =
  let
    step' s = do
      r <- step s
      case r of
        VS.Yield x s' -> do
          mb <- f x
          return $
            case mb of
              Nothing ->
                VS.Skip s'
              Just y ->
                VS.Yield y s'
        VS.Skip s' ->
          return $ VS.Skip s'
        VS.Done ->
          return $ VS.Done
    {-# INLINE [0] step' #-}
  in
    VS.Stream step' t
{-# INLINE [1] mapMaybeM #-}

mapMaybe :: (a -> Maybe b) -> VS.Stream Id a -> VS.Stream Id b
mapMaybe f = mapMaybeM (return . f)
{-# INLINE [1] mapMaybe #-}

imapMaybeM :: Monad m => (Int -> a -> m (Maybe b)) -> VS.Stream m a -> VS.Stream m b
imapMaybeM f = mapMaybeM (uncurry f) . VS.indexed
{-# INLINE [1] imapMaybeM #-}

imapMaybe :: (Int -> a -> Maybe b) -> VS.Stream Id a -> VS.Stream Id b
imapMaybe f = mapMaybe (uncurry f) . VS.indexed
{-# INLINE [1] imapMaybe #-}
