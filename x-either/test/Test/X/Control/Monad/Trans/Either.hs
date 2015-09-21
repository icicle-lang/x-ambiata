{-# LANGUAGE TemplateHaskell #-}
module Test.X.Control.Monad.Trans.Either where

import           Control.Applicative ( (<$>), (<*>), pure )
import           Data.Functor.Identity

import           Test.QuickCheck
import           Test.QuickCheck.Function

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

return []
tests :: IO Bool
tests = $quickCheckAll
