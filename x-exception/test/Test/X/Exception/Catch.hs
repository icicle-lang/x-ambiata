{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.X.Exception.Catch (tests) where

import           Control.Monad (return)
import           Control.Applicative
import           Control.Monad.Catch hiding (finally)

import           Data.IORef
import           Data.Eq
import           Data.Function
import           Data.Bool
import           Data.Text
import           Data.Either

import           Disorder.Core.IO

import           System.IO
import           System.IO.Error hiding (catchIOError)

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           X.Exception.Catch

prop_bracketF :: [Text] -> Text -> Property
prop_bracketF l action = action /= "" ==> testIO $ do
  let after' = \_ -> pure $ Right ()
  let action' = (\l' -> pure $ action : l')
  (=== action : l) <$> bracketF (pure l) after' action'

prop_bracketF_failure :: [Text] -> Text -> Text -> Property
prop_bracketF_failure l action failure = action /= "" ==> testIO $ do
  let after' = \_ -> pure $ Left failure
  let action' = (\_ -> pure action)
  (=== failure) <$> bracketF (pure l) after' action'

prop_bracket :: [Text] -> Text -> Text -> Property
prop_bracket l final action = final /= "" && action /= "" ==> testIO $ do
  r <- newIORef l
  let after' = (flip modifyIORef (final :))
  let action' = (flip modifyIORef (action :))
  unsafeBracket (return r) after' action'
  (=== final : action : l) <$> readIORef r

prop_bracket_catch :: [Text] -> Text -> Property
prop_bracket_catch l final = final /= "" ==> testIO $ do
  r <- newIORef l
  let after' = (flip modifyIORef (final :))
  let action' = const $ throwM (userError "")
  _ <- unsafeBracket (return r) after' action' `catchIOError` (const $ return ())
  (=== final : l) <$> readIORef r

prop_bracket_ :: Text -> Text -> Text -> Property
prop_bracket_ before action after = testIO $ do
  r <- newIORef ""
  let before' = writeIORef r before
  let action' = writeIORef r action
  let after' = writeIORef r after
  unsafeBracket_ before' after' action'
  (=== after) <$> readIORef r

prop_bracket__catch :: Text -> Text -> Text -> Property
prop_bracket__catch initial before after = testIO $ do
  r <- newIORef initial
  let before' = writeIORef r before
  let action' = throwM (userError "")
  let after' = writeIORef r after
  unsafeBracket_ before' after' action' `catchIOError` (const $ return ())
  (=== after) <$> readIORef r

prop_finally :: Text -> Text -> Text -> Property
prop_finally initial action after = testIO $ do
  r <- newIORef initial
  let action' = writeIORef r action
  let after' = writeIORef r after
  unsafeFinally action' after'
  (=== after) <$> readIORef r

prop_finally_catch :: Text -> Text -> Property
prop_finally_catch initial after = testIO $ do
  r <- newIORef initial
  let action' = throwM (userError "")
  let after' = writeIORef r after
  unsafeFinally action' after' `catchIOError` (const $ return ())
  (=== after) <$> readIORef r

return []
tests :: IO Bool
tests = $quickCheckAll
