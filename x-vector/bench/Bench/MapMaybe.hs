{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bench.MapMaybe where

import qualified X.Data.Vector.Unboxed as Unboxed
import qualified X.Data.Vector.Stream  as Stream
import qualified Data.List as List
import qualified Data.Maybe as Maybe

import           P


maybeFun :: Int -> Maybe Int
maybeFun i
 = if   i `mod` 2 == 0
   then Just i
   else Nothing
{-# INLINE maybeFun #-}


maybeStream :: Unboxed.Vector Int -> Int
maybeStream xs
 = sum'
 $ Stream.mapMaybe maybeFun
 $ Stream.mapMaybe maybeFun
 $ Stream.streamOfVector xs
 where
  sum' = Stream.unId . Stream.foldl' (+) 0
  {-# INLINE sum' #-}

maybeVector :: Unboxed.Vector Int -> Int
maybeVector xs
 = Unboxed.sum
 $ Unboxed.mapMaybe maybeFun
 $ Unboxed.mapMaybe maybeFun xs

maybeList :: [Int] -> Int
maybeList xs 
 = List.sum
 $ Maybe.mapMaybe maybeFun
 $ Maybe.mapMaybe maybeFun xs

