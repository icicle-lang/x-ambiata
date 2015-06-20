import           Disorder.Core.Main
import qualified Test.X.Data.Attoparsec.ByteString
import qualified Test.X.Data.Attoparsec.Text

main :: IO ()
main =
  disorderMain [
      Test.X.Data.Attoparsec.ByteString.tests
    , Test.X.Data.Attoparsec.Text.tests
    ]
