{-# language BangPatterns #-}
{-# language DataKinds, GADTs, KindSignatures #-}
{-# language TypeOperators #-}
{-# language ScopedTypeVariables #-}
module Data.HVector
  ( HVector
  , nil
  , cons
  , toHList
  , fromHList
  )
where

import Control.Monad.ST (ST)
import Data.HList (HList)
import qualified Data.HList as HList
import Data.Kind (Type)
import Data.Singletons.Types (STypes)
import qualified Data.Singletons.Types as Types
import Data.Type.Equality ((:~:)(..))
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as Mutable
import GHC.Exts (Any)
import Unsafe.Coerce (unsafeCoerce)

newtype HVector :: [Type] -> Type where
  HVector :: Vector Any -> HVector ts

nil :: HVector '[]
nil = HVector mempty

cons :: t -> HVector ts -> HVector (t ': ts)
cons x (HVector xs) = HVector (Vector.cons (unsafeCoerce x :: Any) xs)

toHList :: forall ts. HVector ts -> HList ts
toHList (HVector v) =
  if Vector.length v == 0
  then
    case unsafeCoerce Refl :: ts :~: '[] of
      Refl ->
        HList.Nil
  else
    case unsafeCoerce Refl :: ts :~: (u ': us) of
      Refl ->
        let
          !h = unsafeCoerce (Vector.head v) :: u
          t = toHList (HVector (Vector.tail v) :: HVector us)
        in
          HList.Cons h t

fromHList :: STypes ts -> HList ts -> HVector ts
fromHList ts xs =
  let
    tsLen = sListLength ts
  in
    HVector (Vector.create $ do
      mv <- Mutable.new tsLen
      writeAll mv 0 xs
      pure mv
    )
  where
    sListLength :: forall us. STypes us -> Int
    sListLength = go 0
      where
        go :: forall vs. Int -> STypes vs -> Int
        go !acc vs =
          case vs of
            Types.SNil -> acc
            Types.SCons _ rest -> go (1 + acc) rest

    writeAll :: MVector s Any -> Int -> HList us -> ST s ()
    writeAll mv !ix xs' =
      case xs' of
        HList.Nil ->
          pure ()
        HList.Cons u us -> do
          Mutable.write mv ix (unsafeCoerce u :: Any)
          writeAll mv (ix+1) us
