{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE LambdaCase #-}
module X.Data.Vector.Stream.Enum
  ( enumFromM
  , enumFrom
  ) where

import qualified Data.Vector.Fusion.Stream.Monadic as VS
import Data.Vector.Fusion.Util (Id(..))

import           P

import GHC.Enum (Enum(succ))


enumFromM :: (Enum a, Monad m) => a -> VS.Stream m a
enumFromM start
 = VS.Stream go start
 where
  go s
   = return $ VS.Yield s (succ s)
  {-# INLINE [0] go #-}
{-# INLINE [1] enumFromM #-}

enumFrom :: (Enum a) => a -> VS.Stream Id a
enumFrom = enumFromM
{-# INLINE [1] enumFrom #-}


