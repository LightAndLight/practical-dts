{-# language AllowAmbiguousTypes #-}
{-# language BangPatterns #-}
{-# language DataKinds, GADTs, KindSignatures, PolyKinds #-}
{-# language RoleAnnotations #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
{-# language TypeApplications #-}
{-# language TypeOperators #-}
module Data.Vector.Sized
  ( Vector, nil, cons
  , length
  , null
  , (!)
  , head
  , last
  , Range, rempty, rgrow, rshift
  , toRange, fromRange
  , slice
  , init
  , tail
  , take
  , drop
  , splitAt
  , empty
  , singleton
  , replicate
  , generate
  , iterateN
  , Match(..)
  , match
  , toList
  , fromList
  , append
  )
where

import Prelude hiding (drop, head, init, length, last, null, replicate, splitAt, tail, take)

import Control.Monad.ST (ST)
import Data.Fin.Strict (Fin)
import qualified Data.Fin.Strict as Fin
import qualified Data.Fin.Strict.Internal as Fin.Internal
import Data.Kind (Type)
import Data.List.Sized (List(..))
import Data.Nat (Nat(..))
import qualified Data.Nat as Nat
import qualified Data.Singletons.Nat.Strict as Strict
import qualified Data.Singletons.Nat.Strict.Internal as Strict.Internal
import Data.Type.Equality ((:~:)(..))
import qualified Data.Vector as Vector
import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as Mutable
import Unsafe.Coerce (unsafeCoerce)

newtype Vector :: Nat -> Type -> Type where
  Vector :: Vector.Vector a -> Vector n a
type role Vector nominal nominal
deriving instance Eq a => Eq (Vector n a)
deriving instance Show a => Show (Vector n a)

nil :: Vector 'Zero a
nil = Vector mempty

cons :: a -> Vector n a -> Vector ('Suc n) a
cons a (Vector v) = Vector (Vector.cons a v)

length :: Vector n a -> Strict.SNat n
length (Vector v) = Strict.Internal.SNat (Vector.length v)

(!) :: Vector n a -> Fin n -> a
(!) (Vector v) ix = Vector.unsafeIndex v (Fin.toInt ix)

head :: Vector ('Suc n) a -> a
head (Vector v) = Vector.head v

last :: Vector ('Suc n) a -> a
last (Vector v) = Vector.last v

null :: forall n a. Vector n a -> Nat.IsZero n
null (Vector v) =
  if Vector.null v
  then
    case unsafeCoerce Refl :: n :~: 'Zero of
      Refl ->
        Nat.IsZero
  else
    case unsafeCoerce Refl :: n :~: 'Suc k of
      Refl ->
        Nat.IsSuc

data Range :: Nat -> Nat -> Nat -> Type where
  Range :: !Int -> !Int -> Range ix len n

rempty :: Range 'Zero 'Zero n
rempty = Range 0 0

rgrow :: Range ix len n -> Range ix ('Suc len) ('Suc n)
rgrow (Range ix len) =
  let
    len' = len + 1
  in
    Range ix len'

rshift :: Range ix len n -> Range ('Suc ix) len ('Suc n)
rshift (Range ix len) =
  let
    ix' = ix + 1
  in
    Range ix' len

toRange :: Strict.SNat ix -> Strict.SNat len -> Strict.SNat n -> Maybe (Range ix len n)
toRange ix len n =
  let
    ix' = Strict.toInt ix
    len' = Strict.toInt len
  in
    if ix' + len' <= Strict.toInt n
    then
      Just $ Range ix' len'
    else
      Nothing

fromRange :: Range ix len n -> (Strict.SNat ix, Strict.SNat len)
fromRange (Range ix len) = (Strict.Internal.SNat ix, Strict.Internal.SNat len)

slice :: Range ix len n -> Vector n a -> Vector len a
slice (Range ix len) (Vector v) = Vector (Vector.unsafeSlice ix len v)

init :: Vector ('Suc n) a -> Vector n a
init (Vector v) = Vector (Vector.unsafeInit v)

tail :: Vector ('Suc n) a -> Vector n a
tail (Vector v) = Vector (Vector.unsafeTail v)

take :: Strict.SNat m -> Vector (Nat.Add m n) a -> Vector m a
take count (Vector v) = Vector (Vector.unsafeTake (Strict.toInt count) v)

drop :: Strict.SNat m -> Vector (Nat.Add m n) a -> Vector n a
drop count (Vector v) = Vector (Vector.unsafeDrop (Strict.toInt count) v)

splitAt :: forall m n a. Strict.SNat m -> Vector (Nat.Add m n) a -> (Vector m a, Vector n a)
splitAt count v = (take @m @n count v, drop @m @n count v)

empty :: Vector 'Zero a
empty = nil

singleton :: a -> Vector ('Suc 'Zero) a
singleton a = Vector (Vector.singleton a)

replicate :: Strict.SNat n -> a -> Vector n a
replicate count a = Vector (Vector.replicate (Strict.toInt count) a)

generate :: Strict.SNat n -> (Fin n -> a) -> Vector n a
generate count f =
  Vector (Vector.generate (Strict.toInt count) (f . Fin.Internal.Fin))

iterateN :: Strict.SNat n -> (a -> a) -> a -> Vector n a
iterateN count f a =
  Vector (Vector.iterateN (Strict.toInt count) f a)

data Match :: Nat -> Type -> Type where
  MNil :: Match 'Zero a
  MCons :: a -> Vector n a -> Match ('Suc n) a

match :: forall n a. Vector n a -> Match n a
match (Vector v) =
  if Vector.length v == 0
  then
    case unsafeCoerce Refl :: n :~: 'Zero of
      Refl ->
        MNil
  else
    case unsafeCoerce Refl :: n :~: 'Suc k of
      Refl ->
        let
          !h = Vector.head v
          !t = Vector.tail v
        in
          MCons h (Vector t)

toList :: forall n a. Vector n a -> List n a
toList (Vector v) =
  if Vector.length v == 0
  then
    case unsafeCoerce Refl :: n :~: 'Zero of
      Refl ->
        Nil
  else
    case unsafeCoerce Refl :: n :~: 'Suc k of
      Refl ->
        let
          !h = Vector.head v
          !t = Vector.tail v
        in
          Cons h (toList (Vector t :: Vector k a))

fromList :: forall n a. Strict.SNat n -> List n a -> Vector n a
fromList size v =
  Vector (Vector.create $ do
    mv <- Mutable.new (Strict.toInt size)
    go mv 0 v
    pure mv
  )
  where
    go :: forall s k. MVector s a -> Int -> List k a -> ST s ()
    go mv !ix v' =
      case v' of
        Nil -> pure ()
        Cons a rest -> do
          Mutable.write mv ix a
          go mv (ix+1) rest

append :: Vector m a -> Vector n a -> Vector (Nat.Add m n) a
append (Vector v) (Vector u) = Vector (v <> u)
