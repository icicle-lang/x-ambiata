{-# LANGUAGE RankNTypes #-}
module X.Control.Monad.Trans.Either (
    module X
  , firstEitherT
  , secondEitherT
  , eitherTFromMaybe
  , hoistEitherT
  , joinErrors
  , joinErrorsEither
  , runOrFail
  , expectLeft
  , expectRight
  ) where

import           Control.Monad.Trans.Either as X (
                     EitherT (..)
                   , eitherT
                   , bimapEitherT
                   , left
                   , right
                   , hoistEither
                   , mapEitherT
                   , swapEitherT
                   )

import            Control.Monad
import            Data.Bifunctor
import            Data.Text (Text, unpack)

firstEitherT :: Functor f => (e -> e') -> EitherT e f a -> EitherT e' f a
firstEitherT f =
  bimapEitherT f id

secondEitherT :: Functor f => (a -> a') -> EitherT e f a -> EitherT e f a'
secondEitherT =
  bimapEitherT id

eitherTFromMaybe :: Functor f => e -> f (Maybe a) -> EitherT e f a
eitherTFromMaybe e =
  EitherT . fmap (maybe (Left e) Right)

hoistEitherT :: (forall t. m t -> n t) -> EitherT e m a -> EitherT e n a
hoistEitherT f = EitherT . f . runEitherT

-- | unify the errors of 2 nested EithersT
joinErrors :: (Functor m, Monad m) => (e -> g) -> (f -> g) -> EitherT e (EitherT f m) a -> EitherT g m a
joinErrors g1 g2 = mapEitherT (fmap (join . bimap g2 (first g1))) . runEitherT

-- | unify the errors of 2 nested EithersT with an Either e f
--   note that the "inner" monad error (like a network error) becomes the Left error
--   and that the "outer" error (like a user error) becomes the Right error
joinErrorsEither :: (Functor m, Monad m) => EitherT e (EitherT f m) a -> EitherT (Either f e) m a
joinErrorsEither = joinErrors Right Left


-- |
-- Great for testing. Allows you to keep all properties in EitherT all the way to the top.
-- Great for testIO:
-- prop_... = testIO . runOrFail $ do <all EitherT here>
--
runOrFail :: (Monad m) => EitherT Text m a -> m a
runOrFail = (=<<) (either (fail . unpack) return) . runEitherT

-- |
-- Useful to check an EitherT is Right, eg:
--   expectRight (awsErrorRender) $ runAWST ...
--
expectRight :: Functor m => (l -> Text) -> EitherT l m a -> EitherT Text m a
expectRight err e = bimapEitherT (err) id $ e

-- |
-- Useful to check that an EitherT deliberately failed, and that is ok. eg:
--   expectLeft "This shouldn't be allowed" $ runAWST...
--
expectLeft :: Functor m => f -> EitherT b m e -> EitherT f m b
expectLeft err e =  bimapEitherT (const err) id $ swapEitherT e
