{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module X.Exception.Catch (
    bracketF
  , bracketEitherT'
  ) where

import           Control.Applicative (pure, (<$>))
import           Control.Monad ((>>=))
import           Control.Monad.Catch hiding (finally)
import           Control.Monad.Trans.Either

import           Data.Either
import           Data.Function

import           System.IO


data BracketResult a =
    BracketOk a
  | BracketFailedFinalizerOk SomeException
  | BracketFailedFinalizerError a

-- Bracket where you care about the output of the finalizer. If the finalizer fails
-- with a value level fail, it will return the result of the finalizer.
-- Finalizer:
--  - Left indicates a value level fail.
--  - Right indicates that the finalizer has a value level success, and its results can be ignored.
--
bracketF :: IO a -> (a -> IO (Either b c)) -> (a -> IO b) -> IO b
bracketF a f g =
  mask $ \restore -> do
    a' <- a
    x <- restore (BracketOk <$> g a') `catchAll`
           (\ex -> either BracketFailedFinalizerError (const $ BracketFailedFinalizerOk ex) <$> f a')
    case x of
      BracketFailedFinalizerOk ex ->
        throwM ex
      BracketFailedFinalizerError b ->
        pure b
      BracketOk b -> do
        z <- f a'
        pure $ either id (const b) z

--
-- Exception and `Left` safe version of bracketEitherT.
--
bracketEitherT' :: EitherT e IO a -> (a -> EitherT e IO c) -> (a -> EitherT e IO b) -> EitherT e IO b
bracketEitherT' aquire release run =
  EitherT $ bracketF
    (runEitherT aquire)
    (\r -> case r of
      Left _ ->
        -- Aquire failed, we have nothing to release
        pure . Right $ ()
      Right r' ->
        -- Aquire succeeded, we need to try and release
        runEitherT (release r') >>= \x -> pure $ case x of
          Left err -> Left (Left (err))
          Right _ -> Right ())
    (\r -> case r of
      Left err ->
        -- Aquire failed, we have nothing to run
        pure . Left $ err
      Right r' ->
        -- Aquire succeeded, we can do some work
        runEitherT (run r'))
