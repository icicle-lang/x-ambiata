import           Disorder.Core.Main

import qualified Test.X.Data.Vector.Generic
import qualified Test.X.Data.Vector.Stream.Conversion


main :: IO ()
main =
  disorderMain [
      Test.X.Data.Vector.Generic.tests
    , Test.X.Data.Vector.Stream.Conversion.tests
    ]
