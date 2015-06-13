import           Disorder.Core.Main

import qualified Test.X.Control.Monad.Catch


main :: IO ()
main =
  disorderMain [
      Test.X.Control.Monad.Catch.tests
    ]
