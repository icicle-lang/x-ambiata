{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.X.Framework.Language.Haskell.TH (
    qint
  , qint'
  ) where

import           Data.Either
import           Data.Int
import           Data.Maybe
import           Data.String
import qualified Data.Text as T

import           Text.Read

import           Language.Haskell.TH.Quote
import           X.Language.Haskell.TH

qint :: QuasiQuoter
qint =
  qmaybe (\t -> (readMaybe :: String -> Maybe Int) (T.unpack t))

qint' :: QuasiQuoter
qint' =
  qeither (\t -> (readEither :: String -> Either String Int) (T.unpack t))
