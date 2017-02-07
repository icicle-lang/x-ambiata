{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module X.Data.Aeson.TH (
    embedJson
  ) where

import           Data.Aeson (Value (..), object, decode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V

import           Language.Haskell.TH (ExpQ)
import           Language.Haskell.TH.Syntax (Q, Exp (..), Lit (..),  qAddDependentFile, runIO)

import qualified Prelude (error)
import           P

import           System.IO (FilePath)

embedJson :: FilePath -> Q Exp
embedJson fp = do
  qAddDependentFile fp
  bs <- runIO $ BS.readFile fp
  case (decode . LBS.fromChunks $ return bs) of
    Nothing ->
      Prelude.error $ "Failed to decode embedded json: " <> fp
    Just (m :: Value) ->
      toExp m

toExp :: Value -> ExpQ
toExp (String t) =
  let tt = T.unpack t
  in [|String (T.pack tt)|]
toExp (Null) =
  [|Null|]
toExp (Object objs) =
  [|object $jsList|]
    where
      jsList :: ExpQ
      jsList = ListE <$> mapM objs2list (HM.toList objs)

      objs2list :: (Text, Value) -> ExpQ
      objs2list (key, value) =
        let k = T.unpack key
        in [|(T.pack k, $(toExp value))|]
toExp (Array arr) =
  let arr' = V.toList arr
  in [|Array $ V.fromList $(ListE <$> mapM toExp (arr'))|]
toExp (Number n) =
  [|Number (fromRational $(return $ LitE $ RationalL (toRational n)))|]
toExp (Bool b) =
  [|Bool b|]
