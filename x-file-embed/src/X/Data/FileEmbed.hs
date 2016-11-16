{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module X.Data.FileEmbed (
    embedWhen
  , pairToExp
  , module X
  ) where

import           Language.Haskell.TH.Syntax (Q, Exp(..), Lit(..), runIO, qAddDependentFile)

import           Data.ByteString (ByteString)
import           Data.FileEmbed as X

import           P

import           System.IO (FilePath)


--- | Embed all the files in a directory that match a predicate, recursively.
embedWhen :: (FilePath -> Bool) -> FilePath -> Q Exp
embedWhen p dir = do
  typ <- [t| [(FilePath, ByteString)] |]
  files <- runIO (getDir dir)
  efiles <- mapM (pairToExp dir) $ filter (p . fst) files
  return $
    SigE (ListE efiles) typ

-- Not exposed from Data.FileEmbed
pairToExp :: FilePath -> (FilePath, ByteString) -> Q Exp
pairToExp root (path, bs) = do
  qAddDependentFile $ root <> ('/' : path)
  exp' <- bsToExp bs
  return $!
    TupE [LitE $ StringL path, exp']
