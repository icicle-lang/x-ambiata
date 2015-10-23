import           Disorder.Core.Main

import qualified Test.X.Control.Monad.Trans.Resource
import qualified Test.X.Data.Conduit.Binary

main :: IO ()
main =
  disorderMain [
      Test.X.Control.Monad.Trans.Resource.tests
    , Test.X.Data.Conduit.Binary.tests
    ]
