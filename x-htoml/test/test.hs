import           Orphanarium.Core.Main

import qualified X.Test.Text.Toml


main :: IO ()
main =
  orphanariumMain [
      X.Test.Text.Toml.tests
    ]
