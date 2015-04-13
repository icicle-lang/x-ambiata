import           Orphanarium.Core.Main
import qualified X.Test.Data.Aeson

main :: IO ()
main =
  orphanariumMain [
      X.Test.Data.Aeson.tests
    ]
