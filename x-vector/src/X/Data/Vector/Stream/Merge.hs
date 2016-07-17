{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE LambdaCase #-}
module X.Data.Vector.Stream.Merge
  ( MergePullFrom(..)
  , mergePullOrd
  , mergePullJoin
  , mergePullJoinBy
  , mergeList
  , merge
  ) where

import qualified Data.Vector.Fusion.Stream.Monadic as VS

import           P


-- | Which stream to pull from during a merge, and a single value to emit.
-- The value to emit will often be the read value - if pulling from left, emit the left, etc.
data MergePullFrom a
 = MergePullLeft  a
 | MergePullRight a
 | MergePullBoth  a

-- | Merge two ascending streams with given Ord instance.
-- Left-biased: when elements from both inputs are equal, pull from left.
mergePullOrd :: Ord b => (a -> b) -> a -> a -> MergePullFrom a
mergePullOrd f
 = mergePullJoin (\l _ -> MergePullLeft l) f
{-# INLINE mergePullOrd #-}

-- | Merge two ascending streams, using given merge function when two elements are equal
mergePullJoin :: Ord b => (a -> a -> MergePullFrom a) -> (a -> b) -> a -> a -> MergePullFrom a
mergePullJoin f c
 = mergePullJoinBy f (compare `on` c)
{-# INLINE mergePullJoin #-}

-- | Merge two ascending streams with Ordering function, use given merge function when two elements are equal
mergePullJoinBy :: (a -> a -> MergePullFrom a) -> (a -> a -> Ordering) -> a -> a -> MergePullFrom a
mergePullJoinBy f c l r
 = case c l r of
    LT -> MergePullLeft l
    EQ -> f l r
    GT -> MergePullRight r
{-# INLINE mergePullJoinBy #-}


-- | Merge two lists together.
-- If they are already sorted and unique, the result will be sorted and unique union of the two.
-- This is really just here as a specification.
mergeList :: (a -> a -> MergePullFrom a) -> [a] -> [a] -> [a]
mergeList f l r = go l r
 where
  go xs [] = xs
  go [] ys = ys
  go (x:xs) (y:ys)
   = case f x y of
      -- Note that this is effectively pushing back onto the top of 'ys'.
      -- This 'peek' is important for the implementation over streams, below.
      MergePullLeft  x' -> x' : go xs (y:ys)
      MergePullBoth  v' -> v' : go xs ys
      MergePullRight y' -> y' : go (x:xs) ys


-- | Merge two streams together.
-- If they are already sorted and unique, the result will be sorted and unique union of the two.
--
-- This is a fair bit more complicated than the mergeList implementation above, but the idea is the same.
-- The streams themselves have no way of peeking at the head of the stream or putting a value back,
-- so we need to implement that by hand.
-- This is not particularly hard, but it does explode the number of possible states.

{-
merge
 (Stream left-state) (Stream right-state)
State:
 ( Maybe left-state, Maybe right-state
 , Maybe left-peek,  Maybe right-peek )

If we have a *-peek value, we will not pull from *-state.
If *-state is Nothing, the left stream is finished, but a final value may be in *-peek.

Initial state is (Just *-state), with empty peeks.


While there are sixteen possibilities of the Maybes, there are only six real state types:

 merge:
    When we have a value in both peeks, we can compare them together and emit something.
    The peek that is used is thrown away.
 
 read-L / read-R:
    We do not have a value in left/right peek, but left/right stream is not finished.
    Attempt to read from left/right stream.

 fill-L / fill-R:
    We have a value in left/right peek, and the other stream is finished.
    We can emit this value as-is. Eventually the entire leftover stream will be emitted.

 done:
    Both streams are finished, and both peeks are finished.

Here is a picture showing the sixteen possibilities, and which action they relate to.

                                                left-peek
                                  Just              |          Nothing
left-state  | right-state |         |          right-peek         |               |
            |             | Just    |  Nothing      |   Just      |  Nothing      |
------------|-------------|---------|---------------|-------------|---------------|
            |    Just     | merge   | read-R        | read-L      | read-L        |
  Just      | ------------|---------|---------------|-------------|---------------|
            |   Nothing   | merge   | fill-L        | read-L      | read-L        |
------------|-------------|---------|---------------|-------------|---------------|
            |    Just     | merge   | read-R        | fill-R      | read-R        |
 Nothing    | ------------|---------|---------------|-------------|---------------|
            |   Nothing   | merge   | fill-L        | fill-R      | done          |
------------|-------------|---------|---------------|-------------|---------------|

-}
merge :: Monad m => (a -> a -> MergePullFrom a) -> VS.Stream m a -> VS.Stream m a -> VS.Stream m a
merge f (VS.Stream l'step l'state) (VS.Stream r'step r'state)
 = VS.Stream go'step (MergeState (Just l'state) (Just r'state) Nothing Nothing)
 where
  -- I originally had a somewhat neater version that matched against multiple things like:
  --
  -- > go'step (MergeResult ls rs (Just lv) (Just rv)) = doMerge lv rv
  -- > go'step ... = rest...
  --
  -- however, pattern match desugaring ends up producing something like
  --
  -- > fail = rest
  -- > go'step = case ... of
  -- >              Just lv -> case ... of
  -- >                          Just rv -> doMerge
  -- >                          Nothing -> fail
  -- >              Nothing -> fail
  --
  -- and since 'fail' is used multiple times, it won't be inlined into the result.
  -- This doesn't play nicely with SpecConstr, I guess because it doesn't look through bindings,
  -- so the Maybes never get removed.
  --
  -- The lesson here is not to use compound pattern matching in stream transformers.
  --
  go'step m
   = case peekL m of
      Just lv
       -> case peekR m of
           Just rv
            -> doMerge m lv rv
           Nothing
            -> case stateR m of
                Just rs
                 -> doReadR m rs
                Nothing
                 -> doFillL m lv
      Nothing
       -> case stateL m of
           Just ls
            -> doReadL m ls
           Nothing 
            -> case peekR m of
                Just rv
                 -> doFillR m rv
                Nothing
                 -> case stateR m of
                     Just rs
                      -> doReadR m rs
                     Nothing
                      -> return $ VS.Done
  {-# INLINE go'step #-}

  doMerge m lv rv
   = case f lv rv of
      MergePullLeft  a -> return $ VS.Yield a m { peekL = Nothing }
      MergePullRight a -> return $ VS.Yield a m { peekR = Nothing }
      MergePullBoth  a -> return $ VS.Yield a m { peekL = Nothing, peekR = Nothing }
  {-# INLINE doMerge #-}

  doReadL m ls
   = do step <- l'step ls
        case step of
          VS.Yield lv ls' -> return $ VS.Skip m { stateL = Just ls', peekL = Just lv }
          VS.Skip     ls' -> return $ VS.Skip m { stateL = Just ls', peekL = Nothing }
          VS.Done         -> return $ VS.Skip m { stateL = Nothing,  peekL = Nothing }

  {-# INLINE doReadL #-}
  doReadR m rs
   = do step <- r'step rs
        case step of
          VS.Yield rv rs' -> return $ VS.Skip m { stateR = Just rs', peekR = Just rv }
          VS.Skip     rs' -> return $ VS.Skip m { stateR = Just rs', peekR = Nothing }
          VS.Done         -> return $ VS.Skip m { stateR = Nothing,  peekR = Nothing }
  {-# INLINE doReadR #-}
  doFillL m lv
   = return $ VS.Yield lv m { peekL = Nothing }
  {-# INLINE doFillL #-}
  doFillR m rv
   = return $ VS.Yield rv m { peekR = Nothing }
  {-# INLINE doFillR #-}

{-# INLINE merge #-}

data MergeState l r a
 = MergeState
 { stateL :: Maybe l
 , stateR :: Maybe r
 , peekL  :: Maybe a
 , peekR  :: Maybe a }


