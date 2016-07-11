{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE LambdaCase #-}
module X.Data.Vector.Stream.Enum
  ( enumFromM
  , enumFrom
  ) where

import qualified Data.Vector.Fusion.Stream.Monadic as VS

import Data.Functor.Identity (Identity)

import           P

import GHC.Enum (Enum(succ))


enumFromM :: (Enum a, Monad m) => a -> VS.Stream m a
enumFromM start
 = VS.Stream go start
 where
  go s
   = return $ VS.Yield s (succ s)

enumFrom :: (Enum a) => a -> VS.Stream Identity a
enumFrom = enumFromM


