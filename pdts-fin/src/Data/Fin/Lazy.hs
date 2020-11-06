{-# language DataKinds, GADTs, KindSignatures #-}
{-# language StandaloneDeriving #-}
module Data.Fin.Lazy
  ( Fin(..)
  )
where

import Data.Kind (Type)
import Data.Nat (Nat)
import qualified Data.Nat as Nat

data Fin :: Nat -> Type where
  Zero :: Fin ('Nat.Suc n)
  Suc :: Fin n -> Fin ('Nat.Suc n)
deriving instance Show (Fin n)
deriving instance Eq (Fin n)
