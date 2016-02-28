{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module X.Text.Show (
    gshowsPrec
  ) where

import           Data.String (String)

import           GHC.Generics ((:*:)(..), (:+:)(..))
import           GHC.Generics (Constructor(..))
import           GHC.Generics (Generic(..), Rep, Fixity(..))
import           GHC.Generics (U1(..), K1(..), M1(..), D, C, S)
import           GHC.Show (appPrec, appPrec1)

import           P


-- | This implements a generic version of 'Text.Show.showsPrec' which omits
--   labels for record names when rendering constructors.
gshowsPrec :: (Generic a, GShow (Rep a)) => Int -> a -> ShowS
gshowsPrec n = gshowsPrec' ConPrefix n . from


data ConType = ConPrefix | ConInfix String

class GShow f where
  gshowsPrec' :: ConType -> Int -> f a -> ShowS

  isNullary :: f a -> Bool
  isNullary _ =
    False

instance GShow U1 where
  gshowsPrec' _ _ U1 =
    id

  isNullary _ =
    True

instance Show c => GShow (K1 i c) where
  gshowsPrec' _ n (K1 a) =
    showsPrec n a

instance (GShow a, Constructor c) => GShow (M1 C c a) where
  gshowsPrec' _ n c@(M1 x) =
    let
      fixity =
        conFixity c

      t =
        case fixity of
          Prefix ->
            ConPrefix
          Infix _ _ ->
            ConInfix $ conName c
    in
      case fixity of
        Prefix ->
          showParen (n > appPrec && not (isNullary x)) $
            showString (conName c) .
            if isNullary x then id else showChar ' ' .
            gshowsPrec' t appPrec1 x
        Infix _ m ->
          showParen (n > m) $ gshowsPrec' t (m+1) x

instance GShow a => GShow (M1 S s a) where
  gshowsPrec' t n (M1 x) =
    gshowsPrec' t n x

  isNullary (M1 x) =
    isNullary x

instance (GShow a) => GShow (M1 D d a) where
  gshowsPrec' t n (M1 x) =
    gshowsPrec' t n x

instance (GShow a, GShow b) => GShow (a :+: b) where
  gshowsPrec' t n = \case
    L1 x ->
      gshowsPrec' t n x
    R1 x ->
      gshowsPrec' t n x

instance (GShow a, GShow b) => GShow (a :*: b) where
  gshowsPrec' t n (a :*: b) =
    case t of
      ConPrefix ->
        gshowsPrec' t n a .
        showChar ' ' .
        gshowsPrec' t n b
      ConInfix s ->
        let
          isInfixTypeCon = \case
            ':':_ ->
              True
            _ ->
              False

          showBacktick =
            if isInfixTypeCon s then
              id
            else
              showChar '`'
        in
          gshowsPrec' t n a .
          showChar ' ' .
          showBacktick .
          showString s .
          showBacktick .
          showChar ' ' .
          gshowsPrec' t n b
