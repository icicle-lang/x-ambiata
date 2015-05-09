import           Disorder.Core.Main

import qualified Test.X.Exception.Catch

main :: IO ()
main =
  disorderMain [
      Test.X.Exception.Catch.tests
    ]
