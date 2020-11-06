{-# language DataKinds, GADTs, KindSignatures #-}
{-# language RankNTypes, ScopedTypeVariables #-}
{-# language TypeOperators #-}
module Data.Singletons.Nat.Strict
  ( SNat
  , zero
  , suc
  , roll
  , unroll
  , toInt
  , fromInt
  , add
  , mult
  )
where

import Data.Nat (Nat(..))
import qualified Data.Nat as Nat
import qualified Data.Singletons.Nat.Lazy as Lazy
import Data.Singletons.Nat.Strict.Internal (SNat(..))
import Data.Sigma (Exists(..))
import Data.Type.Equality ((:~:)(..))
import Unsafe.Coerce (unsafeCoerce)

zero :: SNat 'Zero
zero = SNat 0

suc :: SNat n -> SNat ('Suc n)
suc (SNat n) = SNat (n+1)

unroll :: forall n. SNat n -> Lazy.SNat n
unroll (SNat n) =
  if n == 0
  then
    case unsafeCoerce Refl :: n :~: 'Zero of
      Refl ->
        Lazy.SZero
  else
    case unsafeCoerce Refl :: n :~: 'Suc k of
      Refl ->
        Lazy.SSuc (unroll (SNat (n-1) :: SNat k))

toInt :: SNat n -> Int
toInt (SNat n) = n

fromInt :: Int -> Maybe (Exists Nat SNat)
fromInt n =
  if n >= 0
  then pure $ Exists (SNat n)
  else Nothing

roll :: Lazy.SNat n -> SNat n
roll n =
  case n of
    Lazy.SZero -> zero
    Lazy.SSuc n' -> suc $! roll n'

add :: SNat m -> SNat n -> SNat (Nat.Add m n)
add (SNat m) (SNat n) = SNat (m + n)

mult :: SNat m -> SNat n -> SNat (Nat.Mult m n)
mult (SNat m) (SNat n) = SNat (m * n)
