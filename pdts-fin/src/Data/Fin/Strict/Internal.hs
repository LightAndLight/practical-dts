{-# language DataKinds, GADTs, KindSignatures #-}
{-# language RoleAnnotations #-}
{-# language StandaloneDeriving #-}
module Data.Fin.Strict.Internal where

import Data.Kind (Type)
import Data.Nat (Nat)

newtype Fin :: Nat -> Type where
  Fin :: Int -> Fin n
type role Fin nominal
deriving instance Show (Fin n)
