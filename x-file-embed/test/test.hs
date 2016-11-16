import           Disorder.Core.Main

import qualified Test.X.Data.FileEmbed

main :: IO ()
main =
  disorderMain [
      Test.X.Data.FileEmbed.tests
    ]
