import           Disorder.Core.Main
import qualified Test.X.Control.Monad.Trans.Either
import qualified Test.X.Control.Monad.Trans.Either.Exit

main :: IO ()
main =
  disorderMain [
      Test.X.Control.Monad.Trans.Either.tests
    , Test.X.Control.Monad.Trans.Either.Exit.tests
    ]
