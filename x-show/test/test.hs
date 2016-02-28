import           Disorder.Core.Main

import qualified Test.X.Text.Show

main :: IO ()
main =
  disorderMain [
      Test.X.Text.Show.tests
    ]
