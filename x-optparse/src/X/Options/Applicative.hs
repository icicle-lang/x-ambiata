{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module X.Options.Applicative (
    module X
  , RunType (..)
  , SafeCommand (..)
  , pOption
  , textRead
  , command'
  , dispatch
  , orDie
  , orDieWithCode
  , safeCommand
  , versionFlag
  , dryRunFlag
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Either

import qualified Data.Attoparsec.Text as A
import           Data.Either
import           Data.Eq
import           Data.Int (Int)
import           Data.Function
import           Data.String (String)
import           Data.Text as T

import           Options.Applicative as X
import           Options.Applicative.Types as X

import           System.IO
import           System.Environment (getArgs)
import           System.Exit

import           Text.Show


data RunType =
    DryRun
  | RealRun
  deriving (Eq, Show)

data SafeCommand a =
    Version
  | RunCommand RunType a
  deriving (Eq, Show)

-- | Turn an attoparsec parser into a ReadM
pOption :: A.Parser a -> ReadM a
pOption p =
  either readerError pure =<< (ReadM . ReaderT $ pure . A.parseOnly p . T.pack)

textRead :: ReadM Text
textRead = fmap T.pack str

-- | A 'command' combinator that adds helper and description in a
--   slightly cleaner way
command' :: String -> String -> Parser a -> Mod CommandFields a
command' label description parser =
  command label (info (parser <**> helper) (progDesc description))

-- | Dispatch multi-mode programs with appropriate helper to make the
--   default behaviour a bit better.
dispatch :: Parser a -> IO a
dispatch p = getArgs >>= \x -> case x of
  [] -> customExecParser (prefs showHelpOnError)  (info (p <**> helper) idm)
  _  -> execParser (info (p <**> helper) idm)

-- | orDieWithCode with an exit code of 1 in case of an error
--
orDie :: (e -> Text) -> EitherT e IO a -> IO a
orDie = orDieWithCode 1

-- | An idiom for failing hard on EitherT errors.
--
-- *This really dies*. There is no other way to say it.
--
-- The reason it lives with command line parser tooling, is that is
-- the only valid place to actually exit like this. Be appropriately
-- wary.
--
orDieWithCode :: Int -> (e -> Text) -> EitherT e IO a -> IO a
orDieWithCode code render e =
  runEitherT e >>=
    either (\err -> (hPutStrLn stderr . T.unpack . render) err >> exitWith (ExitFailure code)) pure

-- | Turn a Parser for a command of type a into a safe command
--   with a dry-run mode and a version flag
safeCommand :: Parser a -> Parser (SafeCommand a)
safeCommand commandParser =
      Version <$ versionFlag
  <|> RunCommand <$> dryRunFlag <*> commandParser

versionFlag :: Parser ()
versionFlag =
  flag' () $
       short 'v'
    <> long "version"
    <> help "Version information"

dryRunFlag :: Parser RunType
dryRunFlag =
  flag RealRun DryRun $
       long "dry-run"
    <> hidden
