import           Disorder.Core.Main

import qualified Test.IO.X.System.Posix.Fcntl

main :: IO ()
main =
  disorderMain [
      Test.IO.X.System.Posix.Fcntl.tests
    ]
