{-# language DataKinds, GADTs, KindSignatures, PolyKinds #-}
{-# language StandaloneDeriving #-}
module Data.List.Sized
  ( List(..)
  , length
  , (!)
  , head
  , last
  , null
  , Range, toRange, fromRange
  , slice
  , replicateA
  , append
  )
where

import Prelude hiding (head, last, length, null)

import Data.Fin.Lazy (Fin)
import qualified Data.Fin.Lazy as Fin
import Data.Nat (Nat(..))
import qualified Data.Nat as Nat
import qualified Data.Singletons.Nat.Lazy as Lazy
import Data.Kind (Type)

data List :: Nat -> Type -> Type where
  Nil :: List 'Zero a
  Cons :: a -> List n a -> List ('Suc n) a
deriving instance Show a => Show (List n a)
deriving instance Eq a => Eq (List n a)

length :: List n a -> Lazy.SNat n
length xs =
  case xs of
    Nil -> Lazy.SZero
    Cons _ xs' -> Lazy.SSuc (length xs')

(!) :: List n a -> Fin n -> a
(!) xs ix =
  case ix of
    Fin.Zero ->
      case xs of
        Cons a _ -> a
    Fin.Suc ix' ->
      case xs of
        Cons _ xs' ->
          (!) xs' ix'

data Range :: Nat -> Nat -> Nat -> Type where
  REmpty :: Range 'Zero 'Zero n
  RGrow :: Range ix len n -> Range ix ('Suc len) ('Suc n)
  RShift :: Range ix len n -> Range ('Suc ix) len ('Suc n)

toRange :: Lazy.SNat ix -> Lazy.SNat len -> Lazy.SNat n -> Maybe (Range ix len n)
toRange ix len total =
  case ix of
    Lazy.SZero ->
      case len of
        Lazy.SZero ->
          pure REmpty
        Lazy.SSuc len' ->
          case total of
            Lazy.SZero ->
              Nothing
            Lazy.SSuc total' ->
              RGrow <$> toRange ix len' total'
    Lazy.SSuc ix' ->
      case total of
        Lazy.SZero ->
          Nothing
        Lazy.SSuc total' ->
          RShift <$> toRange ix' len total'

head :: List ('Nat.Suc n) a -> a
head (Cons a _) = a

last :: List ('Nat.Suc n) a -> a
last (Cons x xs) =
  case xs of
    Nil -> x
    Cons{} -> last xs

null :: List n a -> Nat.IsZero n
null xs =
  case xs of
    Nil -> Nat.IsZero
    Cons{} -> Nat.IsSuc

fromRange :: Range ix len n -> (Lazy.SNat ix, Lazy.SNat len)
fromRange range =
  case range of
    REmpty -> (Lazy.SZero, Lazy.SZero)
    RGrow range' ->
      let
        (ix, len) = fromRange range'
      in
        (ix, Lazy.SSuc len)
    RShift range' ->
      let
        (ix, len) = fromRange range'
      in
        (Lazy.SSuc ix, len)

slice :: Range ix len n -> List n a -> List len a
slice range xs =
  case range of
    REmpty ->
      Nil
    RGrow range' ->
      case xs of
        Cons x xs' ->
          Cons x (slice range' xs')
    RShift range' ->
      case xs of
        Cons _ xs' ->
          slice range' xs'

replicateA :: Applicative m => Lazy.SNat n -> m a -> m (List n a)
replicateA n ma =
  case n of
    Lazy.SZero ->
      pure Nil
    Lazy.SSuc k ->
      Cons <$> ma <*> replicateA k ma

append :: List m a -> List n a -> List (Nat.Add m n) a
append l m =
  case l of
    Nil -> m
    Cons a rest -> Cons a (append rest m)
