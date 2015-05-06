import           Orphanarium.Core.Main

import qualified Test.X.Exception.Catch

main :: IO ()
main =
  orphanariumMain [
      Test.X.Exception.Catch.tests
    ]
