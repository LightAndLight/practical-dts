{-# language GADTs, KindSignatures, PolyKinds #-}
{-# language RankNTypes #-}
{-# language QuantifiedConstraints, StandaloneDeriving #-}
module Data.Sigma
  ( Sigma(..), first, second
  , Exists(..), unpack
  )
where

import Data.Kind (Type)

data Sigma (s :: k -> Type) (x :: k) (p :: k -> Type) :: Type where
  Sigma :: s x -> p x -> Sigma s x p

first :: Sigma s x p -> s x
first (Sigma a _) = a

second :: Sigma s x p -> p x
second (Sigma _ b) = b

data Exists (k :: Type) (p :: k -> Type) :: Type where
  Exists :: forall (x :: k) p. p x -> Exists k p
deriving instance (forall x. Show (p x)) => Show (Exists k p)

unpack :: Exists k p -> (forall x. p x -> r) -> r
unpack (Exists a) f = f a
