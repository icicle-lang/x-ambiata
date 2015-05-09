import           Disorder.Core.Main

import qualified X.Test.Text.Toml


main :: IO ()
main =
  disorderMain [
      X.Test.Text.Toml.tests
    ]
