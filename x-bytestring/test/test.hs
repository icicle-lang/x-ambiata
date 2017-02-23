import           Disorder.Core.Main

import qualified Test.X.Data.ByteString
import qualified Test.X.Data.ByteString.Char8
import qualified Test.X.Data.ByteString.Unsafe

main :: IO ()
main =
  disorderMain [
      Test.X.Data.ByteString.tests
    , Test.X.Data.ByteString.Char8.tests
    , Test.X.Data.ByteString.Unsafe.tests
    ]
