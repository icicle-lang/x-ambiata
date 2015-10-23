{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.X.Control.Monad.Trans.Resource (tests) where

import           Control.Applicative
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Resource

import qualified Data.ByteString as BS
import           Data.Conduit
import           Data.Conduit.Binary as CB
import           Data.Conduit.List as CL
import           Data.Either
import           Data.Text (Text)
import           Data.Void (Void)

import           Disorder.Core.IO

import           Prelude (($), Bool, return)

import           System.IO (IO)
import           System.IO.Temp
import           System.FilePath

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           X.Control.Monad.Trans.Resource ()

prop_monadresource_eithert :: Property
prop_monadresource_eithert = testIO $
  withSystemTempDirectory "conduit" $ \p -> do
    let f = p </> "file"
    BS.writeFile f "xxx"
    r <- runResourceT (runEitherT (runConduit (filterFile f)))
    pure (r === Left ":(")

----------
-- HELPERS
----------

filterFile :: MonadResource m => FilePath -> ConduitM () Void (EitherT Text m) [BS.ByteString]
filterFile fp = CB.sourceFile fp
   =$= CB.lines
   -- filter some lines
   =$= CL.map parse
   -- collect errors in EitherT Text m
   =$= CL.mapM hoistEither
   =$= CL.consume

parse :: BS.ByteString -> Either Text BS.ByteString
parse _ = Left ":("

--
return []
tests :: IO Bool
tests = $quickCheckAll
