import           Disorder.Core.Main

import qualified Test.X.Data.Vector.Generic
import qualified Test.X.Data.Vector.Stream.Conversion
import qualified Test.X.Data.Vector.Stream.Enum
import qualified Test.X.Data.Vector.Stream.Map
import qualified Test.X.Data.Vector.Stream.Merge


main :: IO ()
main =
  disorderMain [
      Test.X.Data.Vector.Generic.tests
    , Test.X.Data.Vector.Stream.Conversion.tests
    , Test.X.Data.Vector.Stream.Enum.tests
    , Test.X.Data.Vector.Stream.Map.tests
    , Test.X.Data.Vector.Stream.Merge.tests
    ]
