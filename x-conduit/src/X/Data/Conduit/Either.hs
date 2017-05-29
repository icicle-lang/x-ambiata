{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module X.Data.Conduit.Either
  ( bindEither
  , concatRights
  , mapRightConcat
  , mapRightE
  , mapRightM_
  ) where


import           Control.Monad.IO.Class (MonadIO, liftIO)

import           Data.Conduit (Conduit)
import qualified Data.Conduit as DC
import qualified Data.Conduit.List as DCL


-- Conduit combinators for working on conduits containing `Either`s.

-- | Standard monadic bind specialized for Either in a conduit.
bindEither :: Monad m => (a -> Either e b) -> Conduit (Either e a) m (Either e b)
bindEither = DCL.map . (=<<)

-- | concat the Right values.
concatRights :: (Monad m, Foldable t) => Conduit (Either e (t a)) m (Either e a)
concatRights =
  DC.awaitForever $ either (DC.yield . Left) (mapM_ (DC.yield . Right))

-- | Apply a function `a -> [b]` in a `concatMap` style to the `Right` values.
mapRightConcat :: (Monad m, Foldable t) => (a -> t b) -> Conduit (Either e a) m (Either e b)
mapRightConcat f =
  DC.awaitForever $ either (DC.yield . Left) (mapM_ (DC.yield . Right) . f)


-- | mapRight ef fab : Map a pure function that may produce `Either` over the
-- `Right` of the input `Either` converting `Left` from the input to `Left` of
-- the output type.
-- This is a more general version of `bindEither`.
mapRightE ::  Monad m => (e -> f) -> (a -> Either f b) -> Conduit (Either e a) m (Either f b)
mapRightE ef fab = DC.awaitForever $ DC.yield . either (Left . ef) fab


-- | Conduit component that allows the `Right` to be say written to a file
-- while the `Left`s (eg warnings) are passed along the conduit.
mapRightM_ :: MonadIO m => (a -> IO (Either e ())) -> Conduit (Either e a) m (Either e a)
mapRightM_ actionR =
  DC.awaitForever $
    either (DC.yield . Left)
        (\a -> liftIO (actionR a) >>= either (DC.yield . Left) (const $ DC.yield (Right a)))
