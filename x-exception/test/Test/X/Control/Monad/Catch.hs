{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.X.Control.Monad.Catch where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either

import           Data.Bool
import           Data.Either
import           Data.Eq
import           Data.Function
import           Data.IORef
import           Data.Monoid
import           Data.Text

import           Disorder.Core.IO

import           System.IO

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           X.Control.Monad.Catch


prop_bracketF :: [Text] -> Text -> Property
prop_bracketF l action = action /= "" ==> testIO $ do
  let after' = const . pure $ Right ()
  let action' = \l' -> pure $ action : l'
  (=== action : l) <$> bracketF (pure l) after' action'

prop_bracketF_failure :: [Text] -> Text -> Text -> Property
prop_bracketF_failure l action failure = action /= "" ==> testIO $ do
  let after' = const . pure $ Left failure
  let action' = \_ -> pure action
  (=== failure) <$> bracketF (pure l) after' action'

prop_bracketEitherT :: Text -> Text -> Text -> Property
prop_bracketEitherT a b c = testIO $ do
  ref <- newIORef ""
  let before' = swazzle ref a
  let after' = \_ -> swazzle ref c
  let action' = \_ -> swazzle ref b
  _ <- runEitherT $ bracketEitherT' before' after' action'
  f <- readIORef ref
  pure $ f === (a <> b <> c)

prop_bracketEitherT_action_failed :: Text -> Text -> Text -> Property
prop_bracketEitherT_action_failed a b c = testIO $ do
  ref <- newIORef ""
  let before' = swazzle ref a
  let after' = \_ -> swazzle ref c
  let action' = \_ -> swazzle ref b >> left ()
  _ <- runEitherT $ bracketEitherT' before' after' action'
  f <- readIORef ref
  pure $ f === (a <> b <> c)

prop_bracketEitherT_aquire_failed :: Text -> Text -> Text -> Property
prop_bracketEitherT_aquire_failed a b c = testIO $ do
  ref <- newIORef ""
  let before' = swazzle ref a >> left ()
  let after' = \_ -> swazzle ref c
  let action' = \_ -> swazzle ref b
  _ <- runEitherT $ bracketEitherT' before' after' action'
  f <- readIORef ref
  pure $ f === a

swazzle :: IORef Text -> Text -> EitherT () IO ()
swazzle ref t =
  liftIO $ modifyIORef ref (<> t)

return []
tests :: IO Bool
tests = $quickCheckAll
