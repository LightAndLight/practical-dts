{-# language DataKinds, GADTs, KindSignatures, TypeFamilies #-}
{-# language StandaloneDeriving #-}
{-# language UndecidableInstances #-}
module Data.Nat
  ( Nat(..)
  , IsZero(..)
  , Add
  , Mult
  )
where

import Data.Kind (Type)

data Nat = Zero | Suc Nat

type family Add (m :: Nat) (n :: Nat) :: Nat where
  Add 'Zero n = n
  Add ('Suc m) n = 'Suc (Add m n)

type family Mult (m :: Nat) (n :: Nat) :: Nat where
  Mult 'Zero n = 'Zero
  Mult ('Suc m) n = Add n (Mult m n)

data IsZero :: Nat -> Type where
  IsZero :: IsZero 'Zero
  IsSuc :: IsZero ('Suc n)
deriving instance Show (IsZero n)
deriving instance Eq (IsZero n)
