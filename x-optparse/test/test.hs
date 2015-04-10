import           Orphanarium.Core.Main
import qualified X.Test.Options.Applicative

main :: IO ()
main =
  orphanariumMain [
      X.Test.Options.Applicative.tests
    ]
