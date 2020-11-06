{-# language DataKinds, GADTs #-}
{-# language ScopedTypeVariables #-}
{-# language TypeOperators #-}
module Data.Fin.Strict
  ( Fin
  , zero
  , suc
  , toInt
  , unroll
  )
where

import Data.Fin.Strict.Internal
import qualified Data.Fin.Lazy as Lazy
import Data.Nat (Nat(..))
import Data.Type.Equality ((:~:)(..))
import Unsafe.Coerce (unsafeCoerce)

zero :: Fin ('Suc n)
zero = Fin 0

suc :: Fin n -> Fin ('Suc n)
suc (Fin n) = Fin (n+1)

toInt :: Fin n -> Int
toInt (Fin n) = n

unroll :: forall n. Fin n -> Lazy.Fin n
unroll (Fin n) =
  if n == 0
  then
    case unsafeCoerce Refl :: n :~: 'Suc k of
      Refl ->
        Lazy.Zero
  else
    case unsafeCoerce Refl :: n :~: 'Suc k of
      Refl ->
        Lazy.Suc (unroll (Fin (n-1) :: Fin k))
