{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bench.Merge where

import qualified X.Data.Vector.Unboxed as Unboxed
import qualified X.Data.Vector.Stream  as Stream
import qualified Data.List as List

import           P


mergeFun :: Int -> Int -> Stream.MergePullFrom Int
mergeFun = Stream.mergePullOrd id
{-# INLINE mergeFun #-}


mergeStream :: Unboxed.Vector Int -> Unboxed.Vector Int -> Int
mergeStream xs ys
 = sum'
 $ Stream.merge mergeFun
    (Stream.streamOfVector xs)
    (Stream.streamOfVector ys)
 where
  sum' = Stream.unId . Stream.foldl' (+) 0
  {-# INLINE sum' #-}

mergeVector :: Unboxed.Vector Int -> Unboxed.Vector Int -> Int
mergeVector xs ys
 = Unboxed.sum
 $ Unboxed.merge mergeFun xs ys

mergeList :: [Int] -> [Int] -> Int
mergeList xs ys
 = List.sum
 $ Stream.mergeList mergeFun xs ys
 
