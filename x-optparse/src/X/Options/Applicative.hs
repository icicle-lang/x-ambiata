{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module X.Options.Applicative (
    pOption
  , command'
  , dispatch
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Reader

import qualified Data.Attoparsec.Text as A
import           Data.Either
import           Data.Function
import           Data.String (String)
import           Data.Text as T

import           Options.Applicative
import           Options.Applicative.Types

import           System.IO
import           System.Environment (getArgs)

-- | Turn an attoparsec parser into a ReadM
pOption :: A.Parser a -> ReadM a
pOption p =
  either readerError pure =<< (ReadM . ReaderT $ pure . A.parseOnly p . T.pack)

-- | A 'command' combinator that adds helper and description in a slightly cleaner way
command' :: String -> String -> Parser a -> Mod CommandFields a
command' label description parser =
  command label (info (parser <**> helper) (progDesc description))

-- | Dispatch multi-mode programs with appropriate helper to make the default behaviour a bit better.
dispatch :: Parser a -> IO a
dispatch p = getArgs >>= \x -> case x of
  [] -> customExecParser (prefs showHelpOnError)  (info (p <**> helper) idm)
  _  -> execParser (info (p <**> helper) idm)
