{-# LANGUAGE OverloadedStrings #-}
module X.Data.Attoparsec.Text (
    module X
  , parseStringEither
  , parseTextEither
  , positiveIntegerParser
  ) where

import qualified Data.Text as T

import           Data.Attoparsec.Text as X

import           P


-- | Parse a string with no remaining characters
parseStringEither :: Parser a -> String -> Either String a
parseStringEither p s =  parseTextEither p (T.pack s)

-- | Parse some Text with no remaining characters
parseTextEither :: Parser a -> T.Text -> Either String a
parseTextEither p t = eitherResult $ feed (parse p t) ""

-- | parse an Integer that is just a list of digits
positiveIntegerParser :: Parser Integer
positiveIntegerParser =
  (P.readMaybe <$> many1 digit) >>= P.maybe (fail "not a positive integer") pure
