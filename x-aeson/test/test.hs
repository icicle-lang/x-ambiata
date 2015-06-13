import           Disorder.Core.Main
import qualified Test.X.Data.Aeson

main :: IO ()
main =
  disorderMain [
      Test.X.Data.Aeson.tests
    ]
