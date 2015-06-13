{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.X.Options.Applicative where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader

import qualified Data.Attoparsec.Text as A
import           Data.Either
import           Data.Function
import           Data.Maybe
import           Data.Bool
import           Data.Int
import           Data.Text as T

import           X.Options.Applicative

import           P

import           System.IO

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

prop_pOption :: Text -> Property
prop_pOption t =
  let parser = A.string t
      read = pOption parser
      parser' = either (const Nothing) Just . A.parseOnly parser $ t
      read' = either (const Nothing) Just . runExcept . runReaderT (unReadM read) . T.unpack $ t
   in parser' === read'

prop_textRead :: Text -> Property
prop_textRead t =
  -- Converting to Maybe because ParseError doesn't have an Eq instance defined
  (either (const Nothing) Just . runExcept . runReaderT (unReadM textRead) $ T.unpack t) === Just t

prop_safeCommand :: Text -> Property
prop_safeCommand t =
  let arg = fmap T.pack $ argument str $ metavar "argument"
      name = T.filter (/= '-') t
      parser = safeCommand arg
  in  getParseResult (execParserPure (prefs idm) (info parser idm) [T.unpack name, "--dry-run"]) === Just (RunCommand DryRun name)

prop_orDie :: Int -> Property
prop_orDie n =
  ioProperty $ orDie id (pure n) >>= \r -> pure $ r === n

return []
tests :: IO Bool
tests = $quickCheckAll
