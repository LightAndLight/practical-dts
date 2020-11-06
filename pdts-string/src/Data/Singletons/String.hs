{-# language DataKinds, GADTs, KindSignatures #-}
{-# language FlexibleInstances, MultiParamTypeClasses #-}
{-# language MagicHash #-}
{-# language ScopedTypeVariables, TypeApplications #-}
module Data.Singletons.String
  ( SString(..)
  , toString
  )
where

import Data.Kind (Type)
import GHC.Exts (Proxy#, proxy#)
import GHC.OverloadedLabels (IsLabel(..))
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal')

data SString :: Symbol -> Type where
  SString :: KnownSymbol s => SString s
instance Show (SString s) where
  show = show . toString

instance (KnownSymbol s, s ~ t) => IsLabel s (SString t) where; fromLabel = SString

toString :: forall s. SString s -> String
toString SString = symbolVal' (proxy# :: Proxy# s)
