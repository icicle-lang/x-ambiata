{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module X.Options.Applicative (
    pOption
  , textRead
  , command'
  , dispatch
  , orDie
  , envvar
  , envvarHidden
  , tstrOption
  , optionFromText
  , optionFromText'
  , fromEitherText
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Either

import qualified Data.Attoparsec.Text as A
import           Data.Bifunctor ( first )
import           Data.Either
import           Data.Function
import           Data.List
import           Data.Maybe
import           Data.String (String)
import           Data.Text as T

import           Options.Applicative
import           Options.Applicative.Builder.Internal ( HasValue )
import           Options.Applicative.Types

import           P

import           System.IO
import           System.Environment (getArgs)
import           System.Exit

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

-- | An idiom for failing hard on EitherT errors.
--
-- *This really dies*. There is no other way to say it.
--
-- The reason it lives with command line parser tooling, is that is
-- the only valid place to actually exit like this. Be appropriately
-- wary.
--
orDie :: (e -> Text) -> EitherT e IO a -> IO a
orDie render e =
  runEitherT e >>=
    either (\err -> (hPutStrLn stderr . T.unpack . render) err >> exitFailure) pure

-- |
-- sets a default to an environment variable value,
-- unfortunately the usage docs will be a bit crap and they wont mention the env var that it defaults to unless its set (in which case it will show the value also
-- and where it came from, but theres nothing better at this stage unfortunately...
--
-- see <https://github.com/pcapriotti/optparse-applicative/issues/118> .
--
-- so at the moment you probably want to add something to the help message mentioning the environment var name
--
envvar :: (HasValue f) => (a -> T.Text) -> Maybe a -> Mod f a
envvar f = maybe idm (\i -> value i <> showDefaultWith(\d -> "environment variable set: '" ++ T.unpack (f d) ++ "'"))

-- |
-- Like `envvar` but it hides the value of the environment variable in cases where the value might be sensitive (like an AWS key or something) and you
-- don't want to advertise the value in the usage information...
--
envvarHidden :: (HasValue f) => Maybe a -> Mod f a
envvarHidden = maybe idm value

-- parsers

tstrOption :: Mod OptionFields T.Text -> Parser T.Text
tstrOption = option . eitherReader $ pure . T.pack

optionFromText :: (e -> T.Text) -> (T.Text -> Either e a) -> Mod OptionFields a -> Parser a
optionFromText showErr parseText = option . eitherReader $ fromEitherText showErr parseText

optionFromText' :: (e -> T.Text) -> (T.Text -> e) -> (T.Text -> Maybe a) -> Mod OptionFields a -> Parser a
optionFromText' showErr err f = option . eitherReader $ fromEitherText showErr (\t -> maybeToRight (err t) (f t))

fromEitherText :: (e -> T.Text) -> (T.Text -> Either e a) -> String -> Either String a
fromEitherText showErr parseText=
  first (T.unpack . showErr) . parseText . T.pack
