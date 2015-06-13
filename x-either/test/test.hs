import           Disorder.Core.Main
import qualified Test.X.Control.Monad.Trans.Either

main :: IO ()
main =
  disorderMain [
      Test.X.Control.Monad.Trans.Either.tests
    ]
