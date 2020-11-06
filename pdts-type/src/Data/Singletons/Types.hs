{-# language DataKinds, GADTs, KindSignatures #-}
{-# language StandaloneDeriving #-}
{-# language TypeOperators #-}
module Data.Singletons.Types
  ( STypes(..)
  )
where

import Data.Kind (Type)
import Type.Reflection (TypeRep)

data STypes :: [Type] -> Type where
  SNil :: STypes '[]
  SCons :: TypeRep t -> STypes ts -> STypes (t ': ts)
deriving instance Show (STypes ts)
