{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module X.Test.Options.Applicative where

import           Control.Monad
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Except

import qualified Data.Attoparsec.Text as A
import           Data.Either
import           Data.Function
import           Data.Maybe
import           Data.Bool
import           Data.Text as T

import           Options.Applicative.Types

import           System.IO

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           X.Options.Applicative

prop_pOption :: Text -> Property
prop_pOption t =
  let parser = A.string t
      read = pOption parser
      parser' = either (const Nothing) Just . A.parseOnly parser $ t
      read' = either (const Nothing) Just . runExcept . runReaderT (unReadM read) . T.unpack $ t
   in parser' === read'

return []
tests :: IO Bool
tests = $quickCheckAll
