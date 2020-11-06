{-# language DataKinds, GADTs, KindSignatures #-}
{-# language StandaloneDeriving #-}
{-# language RoleAnnotations #-}
module Data.Singletons.Nat.Strict.Internal where

import Data.Kind (Type)
import Data.Nat (Nat)

newtype SNat :: Nat -> Type where
  SNat :: Int -> SNat n
deriving instance Show (SNat n)
type role SNat nominal
