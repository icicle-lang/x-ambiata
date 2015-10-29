{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.X.Control.Monad.Trans.Either where

import           Control.Applicative ((<$>), (<*>), pure)
import           Control.Monad ((>>), return)
import           Control.Monad.IO.Class (liftIO)

import           Data.Bool (Bool)
import           Data.Char (Char)
import           Data.Either (Either(..))
import           Data.Function (($), (.), id)
import           Data.Functor (fmap)
import           Data.Functor.Identity (Identity(..))
import           Data.IORef (IORef, newIORef, readIORef, modifyIORef)
import           Data.Int (Int)
import           Data.Maybe (Maybe(..))
import           Data.Monoid ((<>))
import           Data.String (String)
import           Data.Text (Text)

import           Disorder.Core.IO (testIO)

import           System.IO (IO)

import           Test.QuickCheck (Property, (===), (.&&.), conjoin, quickCheckAll)
import           Test.QuickCheck.Function (Fun, apply)
import           Test.QuickCheck.Instances ()

import           X.Control.Monad.Trans.Either


prop_firstEitherT_id :: String -> Int -> Property
prop_firstEitherT_id s n =
  let (l, r) = (left s :: EitherT String Identity Int, right n :: EitherT String Identity Int)
  in  firstEitherT id l === l .&&.
      firstEitherT id r === r

prop_firstEitherT_map :: Fun String Char -> String -> Property
prop_firstEitherT_map fun s =
  let f = apply fun
      l = left s :: EitherT String Identity Int
  in  firstEitherT f l === left (f s)

prop_secondEitherT_id :: String -> Int -> Property
prop_secondEitherT_id s n =
  let (l, r) = (left s :: EitherT String Identity Int, right n :: EitherT String Identity Int)
  in  secondEitherT id l === l .&&.
      secondEitherT id r === r

prop_secondEitherT_map :: Fun Int Char -> Int -> Property
prop_secondEitherT_map fun n =
  let f = apply fun
      r = right n :: EitherT String Identity Int
  in  secondEitherT f r === right (f n)

prop_eitherTFromMaybeRight :: Int -> Property
prop_eitherTFromMaybeRight t =
  runIdentity (runEitherT $ eitherTFromMaybe () (Identity $ Just t)) === Right t

prop_eitherTFromMaybeLeft :: Int -> Property
prop_eitherTFromMaybeLeft t =
  runIdentity (runEitherT $ eitherTFromMaybe t (Identity (Nothing :: Maybe ()))) === Left t

prop_reduceEitherT :: String -> Property
prop_reduceEitherT e =
  let
    forall' :: [a] -> (a -> Property) -> Property
    forall' cs f = conjoin . fmap f $ cs

    cases :: [EitherT String (EitherT String Identity) String]
    cases = [
        left e                          -- Outer error
      , (EitherT . fmap pure) $ left e  -- Inner error
      , return e                        -- Success
      ]

    identityReduceIsJoin :: EitherT String (EitherT String Identity) String -> Property
    identityReduceIsJoin = (===) <$> reduceEitherT id id <*> joinEitherT id

  in forall' cases identityReduceIsJoin

prop_joinErrorsRight :: String -> Property
prop_joinErrorsRight e =
  joinErrorsEither (left e) === (left (Right e) :: EitherT (Either String String) Identity Int)

prop_joinErrorsLeft :: String -> Property
prop_joinErrorsLeft e =
  joinErrorsEither (EitherT (EitherT (Identity (Left e)))) === (left (Left e) :: EitherT (Either String String) Identity Int)

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
