import           Disorder.Core.Main

import qualified Test.X.Data.Conduit.Binary

main :: IO ()
main =
  disorderMain [
      Test.X.Data.Conduit.Binary.tests
    ]
