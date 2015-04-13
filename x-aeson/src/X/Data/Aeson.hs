module X.Data.Aeson (
    (.=?)
  ) where

import           Data.Aeson
import           Data.Text


(.=?) :: ToJSON a => Text -> Maybe a -> [(Text, Value)]
(.=?) k =
  maybe [] (\x -> [k .= x])
