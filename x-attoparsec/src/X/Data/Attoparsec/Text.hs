{-# LANGUAGE OverloadedStrings #-}
module X.Data.Attoparsec.Text (
    module X
  , positiveIntegerParser
  , positiveIntParser
  , startsWith
  ) where

import qualified Data.Text as T

import           Data.Attoparsec.Text as X

import           P

-- | Return true if the Text starts with something which can be parsed ok
startsWith :: Parser a -> T.Text -> Bool
startsWith p t =
  case parse p t of
    Partial _ -> True
    Done _ _  -> True
    _         -> False

-- | parse an Integer that is just a list of digits
positiveIntegerParser :: Parser Integer
positiveIntegerParser =
  (P.readMaybe <$> many1 digit) >>= P.maybe (fail "not a positive integer") pure

-- | parse an Int that is just a list of digits
positiveIntParser :: Parser Int
positiveIntParser =
  (P.readMaybe <$> many1 digit) >>= P.maybe (fail "not a positive int") pure
