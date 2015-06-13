import           Disorder.Core.Main

import qualified Test.X.Text.Toml


main :: IO ()
main =
  disorderMain [
      Test.X.Text.Toml.tests
    ]
