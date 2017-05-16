import           Disorder.Core.Main

import qualified Test.X.Data.Conduit.Binary
import qualified Test.X.Data.Conduit.Either

main :: IO ()
main =
  disorderMain [
      Test.X.Data.Conduit.Binary.tests
    , Test.X.Data.Conduit.Either.tests
    ]
