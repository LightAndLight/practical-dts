{-# language BangPatterns #-}
{-# language AllowAmbiguousTypes, FunctionalDependencies, MultiParamTypeClasses #-}
{-# language DataKinds, GADTs, KindSignatures #-}
{-# language FlexibleInstances, UndecidableInstances #-}
{-# language RoleAnnotations #-}
{-# language ScopedTypeVariables, TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
module Data.Record
  ( Record
  , Has
  , empty
  , get
  , set
  , extend
  , retract
  , SFields(..)
  )
where

import Data.Kind (Constraint, Type)
import Data.Singletons.String (SString(..))
import qualified Data.Singletons.String as SString
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import GHC.Exts (Any)
import GHC.TypeLits (Symbol, KnownSymbol)
import Type.Reflection (TypeRep, Typeable, typeRep)
import Unsafe.Coerce (unsafeCoerce)

type family ShowFields xs :: Constraint where
  ShowFields '[] = ()
  ShowFields ('(n, t) ': xs) = (KnownSymbol n, Show t, ShowFields xs)

newtype Record :: [(Symbol, Type)] -> Type where
  Record :: Vector Any -> Record fields
type role Record nominal

instance (KnownFields fs, ShowFields fs) => Show (Record fs) where
  show (Record v) = "{" <> go 0 (fieldsVal @fs) <> "}"
    where
      go :: ShowFields fs' => Int -> SFields fs' -> String
      go !ix fs =
        case fs of
          SNil -> mempty
          SCons (n :: SString n) (_ :: TypeRep ty) rest ->
            SString.toString n <> " = " <>
            show (unsafeCoerce (Vector.unsafeIndex v ix) :: ty) <>
            (case rest of
               SNil -> mempty
               SCons{} -> ", "
            ) <>
            go (ix+1) rest

class Has (field :: Symbol) (fields :: [(Symbol, Type)]) (a :: Type) | field fields -> a where
  offset :: Int

instance {-# overlappable #-} Has field rest a => Has field ('(field', a') ': rest) a where
  offset = offset @field @rest @a + 1

instance {-# overlapping #-} Has field ('(field, a) ': rest) a where
  offset = 0

empty :: Record '[]
empty = Record mempty

get :: forall field fields a. Has field fields a => Record fields -> a
get (Record v) =
  unsafeCoerce (Vector.unsafeIndex v $ offset @field @fields @a) :: a

set :: forall field fields a. Has field fields a => a -> Record fields -> Record fields
set val (Record v) = Record $ v Vector.// [(offset @field @fields @a, unsafeCoerce val :: Any)]

extend :: forall field fields a. a -> Record fields -> Record ('(field, a) ': fields)
extend val (Record v) = Record $ Vector.cons (unsafeCoerce val :: Any) v

retract :: Record ('(field, a) ': fields) -> Record fields
retract (Record v) = Record (Vector.tail v)

data SFields :: [(Symbol, Type)] -> Type where
  SNil :: SFields '[]
  SCons :: SString s -> TypeRep a -> SFields fields -> SFields ('(s, a) ': fields)

class KnownFields (fields :: [(Symbol, Type)]) where
  fieldsVal :: SFields fields

instance KnownFields '[] where; fieldsVal = SNil
instance (KnownSymbol n, Typeable t, KnownFields rest) => KnownFields ('(n, t) ': rest) where
  fieldsVal = SCons (SString @n) (typeRep @t) (fieldsVal @rest)
