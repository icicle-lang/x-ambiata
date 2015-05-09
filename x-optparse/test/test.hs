import           Disorder.Core.Main
import qualified X.Test.Options.Applicative

main :: IO ()
main =
  disorderMain [
      X.Test.Options.Applicative.tests
    ]
