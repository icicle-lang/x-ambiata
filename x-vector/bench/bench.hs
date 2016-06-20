{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Criterion.Main
import           Criterion.Types

import qualified Data.List as List
import qualified Data.Map as Map
import           Data.String (String)

import           P

import           System.IO (IO)

import qualified X.Data.Vector as Boxed
import qualified X.Data.Vector.Generic as Generic
import qualified X.Data.Vector.Unboxed as Unboxed


main :: IO ()
main =
    defaultMainWith config .
      fmap (uncurry bgroup) .
      Map.toList .
      Map.fromListWith (flip mappend) .
      fmap (second (:[])) $
      concatMap benchmarks [1000,2000,3000,4000,5000,6000,7000,8000,9000,10000]

config :: Config
config =
  defaultConfig {
      reportFile = Just "dist/build/x-vector-bench.html"
    , csvFile = Just "dist/build/x-vector-bench.csv"
    }

benchmarks :: Int -> [(String, Benchmark)]
benchmarks size =
  withMatrix size $ \list vec ->
    [ ("Data.List.transpose/list", bench (renderSize size) $ nf List.transpose list)
    , ("X.Data.Vector.Generic.transpose/vector", bench (renderSize size) $ nf Generic.transpose vec)
    ] <>
    -- Going to list and back takes forever, so we only include it in smaller benchmarks --
    if size <= 2000 then
      [("Data.List.transpose/vector", bench (renderSize size) $ nf vecListTranspose vec)]
    else
      []

renderSize :: Int -> String
renderSize n =
  show n <> "²"

vecListTranspose :: Boxed.Vector (Unboxed.Vector Int) -> Boxed.Vector (Unboxed.Vector Int)
vecListTranspose =
  Boxed.fromList .
  fmap Unboxed.fromList .
  List.transpose .
  fmap Unboxed.toList .
  Boxed.toList

withMatrix :: Int -> ([[Int]] -> Boxed.Vector (Unboxed.Vector Int) -> a) -> a
withMatrix size f =
  let
    list :: [[Int]]
    list = List.replicate size [0..size]

    vec :: Boxed.Vector (Unboxed.Vector Int)
    vec = fmap Unboxed.fromList $ Boxed.fromList list
  in
    list `deepseq` vec `deepseq` f list vec
