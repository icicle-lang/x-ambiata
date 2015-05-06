{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.X.Exception.Catch (tests) where

import           Control.Monad (return)
import           Control.Monad.Catch hiding (finally)

import           Data.IORef
import           Data.Eq
import           Data.Function
import           Data.Bool
import           Data.Text

import           System.IO
import           System.IO.Error hiding (catchIOError)

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Test.QuickCheck.Monadic

import           X.Exception.Catch

prop_bracket :: [Text] -> Text -> Text -> Property
prop_bracket l final action = final /= "" && action /= "" ==> monadicIO $ do
  z <- run $ do
    r <- newIORef l
    let after' = (flip modifyIORef (final :))
    let action' = (flip modifyIORef (action :))
    unsafeBracket (return r) after' action'
    readIORef r
  stop $ z === (final : action : l)

prop_bracket_catch :: [Text] -> Text -> Property
prop_bracket_catch l final = final /= "" ==> monadicIO $ do
  z <- run $ do
    r <- newIORef l
    let after' = (flip modifyIORef (final :))
    let action' = const $ throwM (userError "")
    _ <- unsafeBracket (return r) after' action' `catchIOError` (const $ return ())
    readIORef r
  stop $ z === (final : l)


prop_bracket_ :: Text -> Text -> Text -> Property
prop_bracket_ before action after = monadicIO $ do
  z <- run $ do
    r <- newIORef ""
    let before' = writeIORef r before
    let action' = writeIORef r action
    let after' = writeIORef r after
    unsafeBracket_ before' after' action'
    readIORef r
  stop $ z === after


prop_bracket__catch :: Text -> Text -> Text -> Property
prop_bracket__catch initial before after = monadicIO $ do
  z <- run $ do
    r <- newIORef initial
    let before' = writeIORef r before
    let action' = throwM (userError "")
    let after' = writeIORef r after
    unsafeBracket_ before' after' action' `catchIOError` (const $ return ())
    readIORef r
  stop $ z === after

prop_finally :: Text -> Text -> Text -> Property
prop_finally initial action after = monadicIO $ do
  z <- run $ do
    r <- newIORef initial
    let action' = writeIORef r action
    let after' = writeIORef r after
    unsafeFinally action' after'
    readIORef r
  stop $ z === after

prop_finally_catch :: Text -> Text -> Property
prop_finally_catch initial after = monadicIO $ do
  z <- run $ do
    r <- newIORef initial
    let action' = throwM (userError "")
    let after' = writeIORef r after
    unsafeFinally action' after' `catchIOError` (const $ return ())
    readIORef r
  stop $ z === after

return []
tests :: IO Bool
tests = $quickCheckAll
