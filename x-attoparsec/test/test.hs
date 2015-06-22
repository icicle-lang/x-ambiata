import           Disorder.Core.Main
import qualified Test.X.Data.Attoparsec.Text

main :: IO ()
main =
  disorderMain [
      Test.X.Data.Attoparsec.Text.tests
    ]
