import           Disorder.Core.Main

import qualified Test.X.Data.Vector.Generic


main :: IO ()
main =
  disorderMain [
      Test.X.Data.Vector.Generic.tests
    ]
