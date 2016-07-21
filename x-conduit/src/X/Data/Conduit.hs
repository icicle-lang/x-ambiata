module X.Data.Conduit (
    yieldM
  ) where


import           Control.Monad.Trans.Class (lift)
import           Data.Conduit (yield, ConduitM(..))

yieldM :: Monad m => m o -> ConduitM i o m ()
yieldM mo = lift mo >>= yield
