import           Disorder.Core.Main
import qualified X.Test.Data.Aeson

main :: IO ()
main =
  disorderMain [
      X.Test.Data.Aeson.tests
    ]
