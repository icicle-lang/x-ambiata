{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.X.Options.Applicative where

import           Control.Monad (return)
import           Control.Monad.Trans.Except (runExcept)
import           Control.Monad.Trans.Reader (runReaderT)

import qualified Data.Attoparsec.Text as A
import           Data.Either (either)
import           Data.Eq ((/=))
import           Data.Function (($), (.), const)
import           Data.Functor (fmap)
import           Data.Maybe (Maybe(..))
import qualified Data.Text as T

import           X.Options.Applicative

import           Test.QuickCheck ((===), quickCheckAll)
import           Test.QuickCheck.Instances ()

prop_pOption t =
  let
    parser = A.string t
    read = pOption parser
    parser' = either (const Nothing) Just . A.parseOnly parser $ t
    read' = either (const Nothing) Just . runExcept . runReaderT (unReadM read) . T.unpack $ t
  in
    parser' === read'

prop_textRead t =
  -- Converting to Maybe because ParseError doesn't have an Eq instance defined
  (either (const Nothing) Just . runExcept . runReaderT (unReadM textRead) $ T.unpack t) === Just t

prop_safeCommand t =
  let
    arg = fmap T.pack $ argument str $ metavar "argument"
    name = T.filter (/= '-') t
    parser = safeCommand arg
  in
    getParseResult (execParserPure (prefs idm) (info parser idm) [T.unpack name, "--dry-run"])
    ===
    Just (RunCommand DryRun name)

prop_unit =
  let
    parser = safeCommand (pure ())
  in
    getParseResult (execParserPure (prefs idm) (info parser idm) [])
    ===
    Just (RunCommand RealRun ())

return []
tests = $quickCheckAll
