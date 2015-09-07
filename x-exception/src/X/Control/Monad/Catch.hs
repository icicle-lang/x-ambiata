{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module X.Control.Monad.Catch (
    bracketF
  , bracketEitherT'
  ) where

import           Control.Monad ((>>=), return, liftM)
import           Control.Monad.Catch hiding (finally)
import           Control.Monad.Trans.Either

import           Data.Either
import           Data.Function
import           Data.Functor ( (<$) )


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
bracketF :: MonadMask m => m a -> (a -> m (Either b c)) -> (a -> m b) -> m b
bracketF a f g =
  mask $ \restore -> do
    a' <- a
    x <- restore (BracketOk `liftM` g a') `catchAll`
           (\ex -> either BracketFailedFinalizerError (const $ BracketFailedFinalizerOk ex) `liftM` f a')
    case x of
      BracketFailedFinalizerOk ex ->
        throwM ex
      BracketFailedFinalizerError b ->
        return b
      BracketOk b -> do
        z <- f a'
        return $ either id (const b) z

--
-- Exception and `Left` safe version of bracketEitherT.
--
bracketEitherT'
  :: forall m e a b c. MonadMask m
  => EitherT e m a
  -> (a -> EitherT e m c)
  -> (a -> EitherT e m b)
  -> EitherT e m b
bracketEitherT' acquire release run =
  let
    acquire' :: m (Either e a)
    acquire' = runEitherT acquire

    release' :: Either e a -> m (Either e c)
    release' = either (return . Left) (runEitherT . release) -- if acquire' failed then pass through

    run' :: Either e a -> m (Either e b) 
    run' = either (return . Left) (runEitherT . run) -- if acquire' failed then skip run.

 in EitherT $ mask $ \unmask -> do
  resource <- acquire'
  result <- unmask (run' resource) `onException` release' resource
  releaseResult <- release' resource
  return $ result >>= (<$ releaseResult)
