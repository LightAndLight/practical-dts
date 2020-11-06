{-# language DataKinds, GADTs #-}
{-# language ScopedTypeVariables #-}
{-# language TemplateHaskell #-}
module Main where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Data.Fin.Strict as Fin.Strict
import Data.Nat (Nat)
import qualified Data.Nat as Nat
import Data.Sigma (Exists(..))
import qualified Data.Singletons.Nat.Lazy as Lazy
import qualified Data.Singletons.Nat.Strict as Strict
import Data.List.Sized (List)
import qualified Data.List.Sized as List
import Data.Vector.Sized (Vector)
import qualified Data.Vector.Sized as Vector

genSNat :: Gen (Exists Nat Lazy.SNat)
genSNat =
  Gen.recursive Gen.choice
  [ pure $ Exists Lazy.SZero ]
  [ do
      Exists n <- genSNat
      pure $ Exists (Lazy.SSuc n)
  ]

genPositiveSNat :: Gen (Exists Nat Lazy.SNat)
genPositiveSNat =
  Gen.recursive Gen.choice
  [ pure $ Exists (Lazy.SSuc Lazy.SZero) ]
  [ do
      Exists n <- genSNat
      pure $ Exists (Lazy.SSuc n)
  ]

genVec :: Lazy.SNat n -> Gen a -> Gen (Vector n a)
genVec n val =
  case n of
    Lazy.SZero ->
      pure Vector.nil
    Lazy.SSuc n' ->
      Vector.cons <$> val <*> genVec n' val

genList :: Lazy.SNat n -> Gen a -> Gen (List n a)
genList n val = List.replicateA n val

genFin :: Lazy.SNat ('Nat.Suc n) -> Gen (Fin.Strict.Fin ('Nat.Suc n))
genFin n =
  Gen.recursive Gen.choice
  [ pure Fin.Strict.zero ]
  [ case n of
     Lazy.SSuc n' -> go n'
  ]
  where
    go :: Lazy.SNat n -> Gen (Fin.Strict.Fin ('Nat.Suc n))
    go n' =
      case n' of
        Lazy.SZero -> pure Fin.Strict.zero
        Lazy.SSuc n'' ->
          Gen.recursive Gen.choice
            [ pure Fin.Strict.zero ]
            [ Fin.Strict.suc <$> go n'' ]

prop_fromList_toList_id :: Property
prop_fromList_toList_id =
  property $ do
    let genVal = Gen.int (Range.constant minBound maxBound)
    Exists (n :: Lazy.SNat n) <- forAll genSNat
    vec :: Vector n Int <- forAll $ genVec n genVal
    Vector.fromList (Strict.roll n) (Vector.toList vec) === vec

prop_toList_fromList_id :: Property
prop_toList_fromList_id =
  property $ do
    let genVal = Gen.int (Range.constant minBound maxBound)
    Exists (n :: Lazy.SNat n) <- forAll genSNat
    list :: List n Int <- forAll $ genList n genVal
    Vector.toList (Vector.fromList (Strict.roll n) list) === list

prop_cons_correct :: Property
prop_cons_correct =
  property $ do
    let genVal = Gen.int (Range.constant minBound maxBound)
    Exists (n :: Lazy.SNat n) <- forAll genSNat
    a :: Int <- forAll genVal
    vec :: Vector n Int <- forAll $ genVec n genVal
    Vector.toList (Vector.cons a vec) === List.Cons a (Vector.toList vec)

prop_length_correct :: Property
prop_length_correct =
  property $ do
    let genVal = Gen.int (Range.constant minBound maxBound)
    Exists (n :: Lazy.SNat n) <- forAll genSNat
    vec :: Vector n Int <- forAll $ genVec n genVal
    Strict.unroll (Vector.length vec) === List.length (Vector.toList vec)

prop_index_correct :: Property
prop_index_correct =
  property $ do
    Exists (n :: Lazy.SNat n) <- forAll genPositiveSNat
    case n of
      -- this case is impossible but I'm too lazy to prove it to the type system
      Lazy.SZero -> discard
      Lazy.SSuc (_ :: Lazy.SNat k) -> do
        let genVal = Gen.int (Range.constant minBound maxBound)
        ix :: Fin.Strict.Fin ('Lazy.Suc k) <- forAll $ genFin n
        vec :: Vector n Int <- forAll $ genVec n genVal
        (vec Vector.! ix) === (Vector.toList vec List.! Fin.Strict.unroll ix)

prop_head_correct :: Property
prop_head_correct =
  property $ do
    Exists (n :: Lazy.SNat n) <- forAll genPositiveSNat
    case n of
      Lazy.SZero -> discard
      Lazy.SSuc (_ :: Lazy.SNat k) -> do
        let genVal = Gen.int (Range.constant minBound maxBound)
        vec :: Vector ('Nat.Suc k) Int <- forAll $ genVec n genVal
        Vector.head vec === List.head (Vector.toList vec)

prop_last_correct :: Property
prop_last_correct =
  property $ do
    Exists (n :: Lazy.SNat n) <- forAll genPositiveSNat
    case n of
      Lazy.SZero -> discard
      Lazy.SSuc (_ :: Lazy.SNat k) -> do
        let genVal = Gen.int (Range.constant minBound maxBound)
        vec :: Vector ('Nat.Suc k) Int <- forAll $ genVec n genVal
        Vector.last vec === List.last (Vector.toList vec)

prop_null_correct :: Property
prop_null_correct =
  property $ do
    let genVal = Gen.int (Range.constant minBound maxBound)
    Exists (n :: Lazy.SNat n) <- forAll genSNat
    vec :: Vector n Int <- forAll $ genVec n genVal
    Vector.null vec === List.null (Vector.toList vec)

main :: IO Bool
main = checkParallel $$discover
