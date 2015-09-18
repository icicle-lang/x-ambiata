{-# LANGUAGE RankNTypes #-}
module X.Control.Monad.Trans.Either (
    module X
  , firstEitherT
  , secondEitherT
  , eitherTFromMaybe
  , hoistEitherT
  , mapEitherE
  , joinEitherT
  , joinErrors
  , joinErrorsEither
  ) where

import           Control.Monad.Trans.Either as X (
                     EitherT (..)
                   , eitherT
                   , bimapEitherT
                   , left
                   , right
                   , hoistEither
                   , mapEitherT
                   )

import            Control.Monad
import            Data.Bifunctor

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

mapEitherE :: Functor m => (Either x a -> Either y b) -> EitherT x m a -> EitherT y m b
mapEitherE f = mapEitherT (fmap f)

joinEitherT :: Functor m => (y -> x) -> EitherT x (EitherT y m) a -> EitherT x m a
joinEitherT f = mapEitherE (join . first f) . runEitherT

-- | unify the errors of 2 nested EithersT
joinErrors :: (Functor m, Monad m) => (x -> z) -> (y -> z) -> EitherT x (EitherT y m) a -> EitherT z m a
joinErrors f g = joinEitherT g . firstEitherT f

-- | unify the errors of 2 nested EithersT with an Either e f
--   note that the "inner" monad error (like a network error) becomes the Left error
--   and that the "outer" error (like a user error) becomes the Right error
joinErrorsEither :: (Functor m, Monad m) => EitherT e (EitherT f m) a -> EitherT (Either f e) m a
joinErrorsEither = joinErrors Right Left
