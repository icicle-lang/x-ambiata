{-# OPTIONS_GHC -fno-warn-orphans #-}
module X.Control.Monad.Trans.Resource where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Resource (MonadResource, liftResourceT)
import Control.Monad.Trans.Either (EitherT)

-- | This instance is useful when adding errors to the monad used for some conduit processing
-- when resources, like files, are involved. For example:
--
-- foo :: MonadResource m => ConduitM () Void (EitherT Text m) [ByteString]
-- foo = CB.sourceFile "src/Main.hs"
--  =$= CB.lines
--  -- filter some lines
--  =$= CL.map parse
--  -- collect errors in EitherT Text m
--  =$= CL.mapM hoistEither
--  =$= CL.consume
--
-- parse :: ByteString -> Either Text ByteString
-- parse _ = Left ":("
instance MonadResource m => MonadResource (EitherT e m) where
  liftResourceT = lift . liftResourceT
