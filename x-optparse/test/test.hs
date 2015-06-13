import           Disorder.Core.Main

import qualified Test.X.Options.Applicative


main :: IO ()
main =
  disorderMain [
      Test.X.Options.Applicative.tests
    ]
