import           Disorder.Core.Main
import qualified Test.X.Data.Attoparsec.ByteString

main :: IO ()
main =
  disorderMain [
      Test.X.Data.Attoparsec.ByteString.tests
    ]
