{-# language DataKinds, GADTs, KindSignatures #-}
{-# language TypeFamilies #-}
{-# language ConstraintKinds, TypeOperators, UndecidableInstances #-}
module Data.HList
  ( All
  , HList(..)
  )
where

import Data.Kind (Constraint, Type)

data HList :: [Type] -> Type where
  Nil :: HList '[]
  Cons :: t -> HList ts -> HList (t ': ts)

type family All c ts :: Constraint where
  All c '[] = ()
  All c (x ': xs) = (c x, All c xs)

instance All Show ts => Show (HList ts) where
  show Nil = "Nil"
  show (Cons a b) = "Cons (" <> show a <> ") (" <> show b <> ")"
