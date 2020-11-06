{-# language DataKinds, GADTs, KindSignatures #-}
{-# language StandaloneDeriving #-}
module Data.Singletons.Nat.Lazy
  ( Nat(..)
  , SNat(..)
  )
where

import Data.Kind (Type)
import Data.Nat (Nat(..))

data SNat :: Nat -> Type where
  SZero :: SNat 'Zero
  SSuc :: SNat n -> SNat ('Suc n)
deriving instance Show (SNat n)
deriving instance Eq (SNat n)
