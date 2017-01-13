{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
module X.Control.Monad.Trans.Either (
  -- * Control.Monad.Trans.Either
    EitherT
  , newEitherT
  , pattern EitherT
  , runEitherT
  , bimapEitherT
  , mapEitherT
  , hoistEither
  , eitherT
  , left
  , right

  -- * Extensions
  , bracketEitherT'
  , firstEitherT
  , secondEitherT
  , eitherTFromMaybe
  , hoistMaybe
  , hoistEitherT
  , mapEitherE
  , joinEitherT
  , joinErrors
  , joinErrorsEither
  , reduceEitherT
  , tryEitherT
  , sequenceEitherT
  , sequenceEither
  ) where

import           Control.Applicative.Lift (Lift (..), Errors, runErrors)
import           Control.Exception.Base (Exception)
import           Control.Monad (Monad(..), (=<<), join)
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Trans.Except (ExceptT(..))
import           Control.Monad.Catch (MonadMask(..), MonadCatch, try)

import           Data.Maybe (Maybe, maybe)
import           Data.Monoid (Monoid(..))
import           Data.Either (Either(..), either)
import           Data.Function (($), (.), id)
import           Data.Functor (Functor(..))
import           Data.Functor.Constant (Constant (..))
import           Data.Traversable (Traversable(..))

import           X.Control.Monad.Catch (bracketF)

------------------------------------------------------------------------
-- Control.Monad.Trans.Either

type EitherT = ExceptT

pattern EitherT m = ExceptT m

runEitherT :: EitherT x m a -> m (Either x a)
runEitherT (ExceptT m) = m
{-# INLINE runEitherT #-}

newEitherT :: m (Either x a) -> EitherT x m a
newEitherT =
  ExceptT
{-# INLINE newEitherT #-}

eitherT :: Monad m => (x -> m b) -> (a -> m b) -> EitherT x m a -> m b
eitherT f g m =
  either f g =<< runEitherT m
{-# INLINE eitherT #-}

left :: Monad m => x -> EitherT x m a
left =
  EitherT . return . Left
{-# INLINE left #-}

right :: Monad m => a -> EitherT x m a
right =
  return
{-# INLINE right #-}

mapEitherT :: (m (Either x a) -> n (Either y b)) -> EitherT x m a -> EitherT y n b
mapEitherT f =
  EitherT . f . runEitherT
{-# INLINE mapEitherT #-}

hoistEither :: Monad m => Either x a -> EitherT x m a
hoistEither =
  EitherT . return
{-# INLINE hoistEither #-}

bimapEitherT :: Functor m => (x -> y) -> (a -> b) -> EitherT x m a -> EitherT y m b
bimapEitherT f g =
  let h (Left  e) = Left  (f e)
      h (Right a) = Right (g a)
  in mapEitherT (fmap h)
{-# INLINE bimapEitherT #-}

------------------------------------------------------------------------
-- Extensions

--
-- Exception and `Left` safe version of bracketEitherT.
--
bracketEitherT' :: MonadMask m => EitherT e m a -> (a -> EitherT e m c) -> (a -> EitherT e m b) -> EitherT e m b
bracketEitherT' acquire release run =
  EitherT $ bracketF
    (runEitherT acquire)
    (\r -> case r of
      Left _ ->
        -- Acquire failed, we have nothing to release
        return . Right $ ()
      Right r' ->
        -- Acquire succeeded, we need to try and release
        runEitherT (release r') >>= \x -> return $ case x of
          Left err -> Left (Left err)
          Right _ -> Right ())
    (\r -> case r of
      Left err ->
        -- Acquire failed, we have nothing to run
        return . Left $ err
      Right r' ->
        -- Acquire succeeded, we can do some work
        runEitherT (run r'))

firstEitherT :: Functor m => (x -> y) -> EitherT x m a -> EitherT y m a
firstEitherT f =
  bimapEitherT f id
{-# INLINE firstEitherT #-}

secondEitherT :: Functor m => (a -> b) -> EitherT x m a -> EitherT x m b
secondEitherT =
  bimapEitherT id
{-# INLINE secondEitherT #-}

eitherTFromMaybe :: Functor m => x -> m (Maybe a) -> EitherT x m a
eitherTFromMaybe x =
  EitherT . fmap (maybe (Left x) Right)
{-# INLINE eitherTFromMaybe #-}

hoistMaybe :: Monad m => x -> Maybe a -> EitherT x m a
hoistMaybe x =
  maybe (left x) return
{-# INLINE hoistMaybe #-}

hoistEitherT :: (forall b. m b -> n b) -> EitherT x m a -> EitherT x n a
hoistEitherT f =
  EitherT . f . runEitherT
{-# INLINE hoistEitherT #-}

mapEitherE :: Functor m => (Either x a -> Either y b) -> EitherT x m a -> EitherT y m b
mapEitherE f =
  mapEitherT (fmap f)
{-# INLINE mapEitherE #-}

joinEitherT :: (Functor m, Monad m) => (y -> x) -> EitherT x (EitherT y m) a -> EitherT x m a
joinEitherT f =
  let first g = either (Left . g) Right
  in mapEitherE (join . first f) . runEitherT
{-# INLINE joinEitherT #-}

-- | unify the errors of 2 nested EithersT
joinErrors :: (Functor m, Monad m) => (x -> z) -> (y -> z) -> EitherT x (EitherT y m) a -> EitherT z m a
joinErrors f g =
  joinEitherT g . firstEitherT f
{-# INLINE joinErrors #-}

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
  => (y -> x)
  -> (forall b. m b -> EitherT y n b)
  -> EitherT x m a
  -> EitherT x n a
reduceEitherT embedError f =
  joinEitherT embedError . hoistEitherT f
{-# INLINE reduceEitherT #-}

-- | unify the errors of 2 nested EithersT with an Either e f
--   note that the "inner" monad error (like a network error) becomes the Left error
--   and that the "outer" error (like a user error) becomes the Right error
joinErrorsEither :: (Functor m, Monad m) => EitherT x (EitherT y m) a -> EitherT (Either y x) m a
joinErrorsEither =
  joinErrors Right Left
{-# INLINE joinErrorsEither #-}

tryEitherT :: (Functor m, MonadCatch m, Exception e) => (e -> x) -> m a -> EitherT x m a
tryEitherT handler = firstEitherT handler . newEitherT . try
{-# INLINE tryEitherT #-}

-- | Lift an 'Either' into 'Errors'.
eitherErrors :: Either e a -> Errors e a
eitherErrors e =
  case e of
    Left es ->
      Other (Constant es)

    Right a ->
      Pure a
{-# INLINE eitherErrors #-}

-- | Like 'sequence', but folding/accumulating all errors in case of a 'Left'.
sequenceEither :: (Monoid x, Traversable t) => t (Either x a) -> Either x (t a)
sequenceEither =
  runErrors . traverse eitherErrors
{-# INLINE sequenceEither #-}

-- | Evaluate each action in sequence, accumulating all errors in case of a failure.
-- Note that this means each action will be run independently, regardless of failure.
sequenceEitherT :: (Monad m, Monoid x, Traversable t) => t (EitherT x m a) -> EitherT x m (t a)
sequenceEitherT es = do
  es' <- lift (traverse runEitherT es)
  hoistEither (sequenceEither es')
{-# INLINE sequenceEitherT #-}
