{-# LANGUAGE OverloadedStrings #-}
module X.Data.Attoparsec.ByteString (
    IsWord(..)
  , ParseTrie(..)
  ) where

import           Data.ByteString (ByteString)
import           Data.Map (Map)

data IsWord =
    IsWord
  | NotWord
  deriving (Eq, Show)

data ParseTrie =
    EmptyParseTrie
  | ParseTrie !IsWord !(Map ByteString ParseTrie)
  deriving (Eq, Show)

