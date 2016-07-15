{-# LANGUAGE NoImplicitPrelude #-}
module X.Data.Vector.Stream
  ( module Stream
  -- * Util.Id:
  -- All the existing stream transformers and conversions work on Id, not Identity.
  -- The short-cut rule "stream (unstream s) = s" also only works on Id, so for
  -- conversions between vectors and streams we really need to use Id.
  , Util.Id(..)
  ) where

import   Data.Vector.Fusion.Stream.Monadic as Stream
import X.Data.Vector.Stream.Conversion     as Stream
import X.Data.Vector.Stream.Enum           as Stream
import X.Data.Vector.Stream.Map            as Stream
import X.Data.Vector.Stream.Merge          as Stream
import   Data.Vector.Fusion.Util           as Util
