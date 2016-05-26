{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.X.Data.Vector.Generic where

import           Data.Functor.Identity (runIdentity)
import qualified Data.List as List

import           P

import           System.IO (IO)

import           Test.QuickCheck (Property, (===), quickCheckAll)
import           Test.QuickCheck.Function (Fun, apply)
import           Test.QuickCheck.Instances ()

import qualified X.Data.Vector as Boxed
import qualified X.Data.Vector.Generic as Generic


prop_mapMaybe :: Fun Int (Maybe Int) -> [Int] -> Property
prop_mapMaybe f =
  equivalent
    (mapMaybe (apply f))
    (Generic.mapMaybe (apply f))

prop_mapMaybeM :: Fun Int (Maybe Int) -> [Int] -> Property
prop_mapMaybeM f =
  equivalent
    (mapMaybe (apply f))
    (runIdentity . Generic.mapMaybeM (pure . apply f))

prop_imapMaybe :: Fun (Int, Int) (Maybe Int) -> [Int] -> Property
prop_imapMaybe f =
  equivalent
    (imapMaybe (curry $ apply f))
    (Generic.imapMaybe (curry $ apply f))

prop_imapMaybeM :: Fun (Int, Int) (Maybe Int) -> [Int] -> Property
prop_imapMaybeM f =
  equivalent
    (imapMaybe (curry $ apply f))
    (runIdentity . Generic.imapMaybeM (curry $ pure . apply f))

imapMaybe :: (Int -> a -> Maybe b) -> [a] -> [b]
imapMaybe f xs =
  catMaybes $ List.zipWith f [0..] xs

equivalent ::
  Eq b =>
  Show b =>
  ([a] -> [b]) ->
  (forall v. (Generic.Vector v a, Generic.Vector v b) => v a -> v b) ->
  [a] ->
  Property
equivalent f g xs =
  f xs === (Boxed.toList . g . Boxed.fromList) xs

return []
tests :: IO Bool
tests = $quickCheckAll
