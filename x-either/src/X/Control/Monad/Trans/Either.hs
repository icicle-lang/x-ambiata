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
  , reduceEitherT
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

-- |
-- `joinErrors` results in a cycle of hoists/mapEitherTs and joinErrors to bubble errors up to the top layer of EitherT
-- before popping it off with a final `runEitherT`, `reduceEitherT` collects the repeated bits in a single function.
--
-- example usage:
--
-- @
-- data ErrorBar = ...
--
-- data ErrorFoo = FooBar ErrorBar | FooBarBar ErrorBarBar | ...
--
-- newtype SomeMonadTransformer1T m a = SomeMonadTransformer1T { run1T :: EitherT ErrorFoo (ReaderT String m) a }
-- newtype SomeMonadTransformer2T m a = SomeMonadTransformer2T { run2T :: EitherT ErrorBar (ReaderT Int m) a }
-- newtype SomeMonadTransformer3T m a = SomeMonadTransformer3T { run3T :: EitherT ErrorBarBar m a }
--
-- myStackUnwrappingFunctionImagineNoEithers :: String -> Int -> SomeMonadTransformer1T (SomeMonadTransformer2T (SomeMonadTransformer3T m)) a -> m a
-- myStackUnwrappingFunctionImagineNoEithers s x = run3T . flip runReaderT x . run2T . flip runReaderT s . run1T
--
-- myStackUnwrappingFunctionWith :: String -> Int -> SomeMonadTransformer1T (SomeMonadTransformer2T (SomeMonadTransformer3T m)) a -> m (Either ErrorFoo a)
-- myStackUnwrappingFunctionWith s x = runEitherT . reduceEitherT FooBarBar (run3T . flip runReaderT x) . reduceEitherT FooBar (run2T . flip runReaderT s) . run1T
--
-- myStackUnwrappingFunctionWithout :: String -> Int -> SomeMonadTransformer1T (SomeMonadTransformer2T m) a -> m (Either ErrorFoo a)
-- myStackUnwrappingFunctionWithout s x = runEitherT . joinEitherT FooBarBar . hoistEitherT (run3T . flip runReaderT x) . joinEitherT FooBar . hoistEitherT (run2T . flip runReaderT s) . run1T
-- @
--
reduceEitherT
  :: (Functor n, Monad n)
  => (e' -> e)
  -> (forall a. m a -> EitherT e' n a)
  -> EitherT e m b
  -> EitherT e n b
reduceEitherT embedError f = joinEitherT embedError . hoistEitherT f

-- | unify the errors of 2 nested EithersT with an Either e f
--   note that the "inner" monad error (like a network error) becomes the Left error
--   and that the "outer" error (like a user error) becomes the Right error
joinErrorsEither :: (Functor m, Monad m) => EitherT e (EitherT f m) a -> EitherT (Either f e) m a
joinErrorsEither = joinErrors Right Left
