{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module X.Options.Applicative (
    module X
  , RunType (..)
  , SafeCommand (..)
  , maybeTextReader
  , eitherTextReader
  , pOption
  , textRead
  , command'
  , dispatch
  , safeCommand
  , versionFlag
  , dryRunFlag
  ) where

import           Control.Monad ((>>=))

import qualified Data.Attoparsec.Text as A
import           Data.Eq (Eq)
import           Data.Either (Either (..), either)
import           Data.Function (($), (.))
import           Data.Functor (fmap)
import           Data.Monoid (mempty)
import           Data.Maybe (Maybe, maybe)
import           Data.String (String)
import           Data.Text (Text)
import qualified Data.Text as T

import           Options.Applicative as X
import           Options.Applicative.Types as X

import           System.IO (IO)
import           System.Environment (getArgs)

import           Text.Show (Show)


data RunType =
    DryRun
  | RealRun
  deriving (Eq, Show)

data SafeCommand a =
    VersionCommand
  | DependencyCommand
  | RunCommand RunType a
  deriving (Eq, Show)

-- | Turn a parser into a ReadM
maybeTextReader :: (Text -> Maybe a) -> ReadM a
maybeTextReader f =
  eitherReader $ \s ->
    maybe (Left $ "Failed to parse: " <> s) pure . f . T.pack $ s

-- | Turn a parser into a ReadM
eitherTextReader :: (e -> Text) -> (Text -> Either e a) -> ReadM a
eitherTextReader render f =
  eitherReader $
    either (Left . T.unpack . render) Right . f . T.pack

-- | Turn apn attoparsec parser into a ReadM
pOption :: A.Parser a -> ReadM a
pOption p =
  eitherReader (A.parseOnly p . T.pack)

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
  [] -> let -- We don't need to see the Missing error if we're getting the whole usage string.
            removeError' (h, e, c) = (h { helpError = mempty }, e, c)
            removeError (Failure (ParserFailure failure)) = Failure (ParserFailure ( removeError' <$> failure ))
            removeError a = a
        in  execParserPure (prefs showHelpOnError) (info (p <**> helper) idm) <$> getArgs
            >>= handleParseResult . removeError
  _  -> execParser (info (p <**> helper) idm)

-- | Turn a Parser for a command of type a into a safe command
--   with a dry-run mode and a version flag
safeCommand :: Parser a -> Parser (SafeCommand a)
safeCommand commandParser =
      VersionCommand <$ versionFlag
  <|> DependencyCommand <$ dependencyFlag
  <|> RunCommand <$> dryRunFlag <*> commandParser

versionFlag :: Parser ()
versionFlag =
  flag' () $
       short 'v'
    <> long "version"
    <> help "Version information"

dependencyFlag :: Parser ()
dependencyFlag =
  flag' () $
       long "dependencies"
    <> hidden

dryRunFlag :: Parser RunType
dryRunFlag =
  flag RealRun DryRun $
       long "dry-run"
    <> hidden
