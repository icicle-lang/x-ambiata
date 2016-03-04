{-# LANGUAGE OverloadedStrings #-}
module X.Data.Attoparsec.Text (
    module X
  , eitherText
  , manyAnd'
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

-- | create a Parser a from a parse result
--   usage: takeWhile (/= '-') >>= eitherText . parseTimestamp
eitherText :: Either T.Text a -> Parser a
eitherText = either (fail . T.unpack) pure

-- | Like 'Data.Attoparsec.Text.manyTill'', but returns the value matched by 
-- the end parser as well.
manyAnd' :: Parser a -> Parser b -> Parser ([a], b)
manyAnd' p end = go
  where
    go = (end >>= (pure . ((,) []))) `mplus` liftM2' f p go

    f x (xs, y) = (x : xs, y)
{-# INLINE manyAnd' #-}
