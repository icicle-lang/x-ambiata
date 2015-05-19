import           Disorder.Core.Main

import qualified Test.X.Language.Haskell.TH

main :: IO ()
main =
  disorderMain [
      Test.X.Language.Haskell.TH.tests
    ]
