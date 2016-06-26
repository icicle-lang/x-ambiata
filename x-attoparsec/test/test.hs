import           Disorder.Core.Main
import qualified Test.X.Data.Attoparsec.Text
import qualified Test.X.Data.Attoparsec.ByteString

main :: IO ()
main =
  disorderMain [
      Test.X.Data.Attoparsec.Text.tests
    , Test.X.Data.Attoparsec.ByteString.tests
    ]
