import           Disorder.Core.Main

import qualified Test.X.Data.ByteString.Unsafe

main :: IO ()
main =
  disorderMain [
      Test.X.Data.ByteString.Unsafe.tests
    ]
