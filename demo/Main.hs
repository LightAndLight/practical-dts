{-# language BangPatterns #-}
{-# language ConstraintKinds #-}
{-# language DataKinds, GADTs, KindSignatures, PolyKinds #-}
{-# language RankNTypes, ScopedTypeVariables #-}
{-# language MultiParamTypeClasses #-}
{-# language StandaloneDeriving #-}
{-# language PatternSynonyms, ViewPatterns #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language RoleAnnotations #-}
module Main where

import Text.Read

import Data.Kind (Type)
import Data.HList (All)
import Data.HVector (HVector)
import qualified Data.HVector as HVector
import Data.Nat (Nat)
import qualified Data.Nat as Nat
import Data.List.Sized (List)
import qualified Data.List.Sized as List
import Data.Sigma (Exists(..))
import qualified Data.Singletons.Nat.Strict as Strict
import Data.Singletons.Types (STypes)
import qualified Data.Singletons.Types as Types
import Data.Vector.Sized (Vector)
import qualified Data.Vector.Sized as Vector
import Type.Reflection ((:~~:)(..), TypeRep, typeRep, eqTypeRep)

parseSNat :: String -> Maybe (Exists Nat Strict.SNat)
parseSNat s = (readMaybe s :: Maybe Int) >>= Strict.fromInt

data TyResult where
  TyResult :: TypeRep a -> Dict (Show a) -> TyResult

parseTy :: String -> Maybe TyResult
parseTy s =
  case s of
    "Bool" -> Just $ TyResult (typeRep @Bool) Dict
    "Int" -> Just $ TyResult (typeRep @Int) Dict
    "String" -> Just $ TyResult (typeRep @String) Dict
    _ -> Nothing

data Dict c where
  Dict :: c => Dict c

data TysResult n where
  TysResult :: STypes ts -> Length ts n -> Dict (All Show ts) -> TysResult n

parseTys :: Vector n String -> Maybe (TysResult n)
parseTys = go . Vector.toList
  where
    go :: List n String -> Maybe (TysResult n)
    go tys =
      case tys of
        List.Nil -> pure $ TysResult Types.SNil LZero Dict
        List.Cons ty rest -> do
          TyResult ty' Dict <- parseTy ty
          TysResult rest' prf Dict <- go rest
          pure $ TysResult (Types.SCons ty' rest') (LSuc prf) Dict

parseOne :: TypeRep a -> String -> Maybe a
parseOne ty s
  | Just HRefl <- eqTypeRep ty (typeRep @Bool) = readMaybe s
  | Just HRefl <- eqTypeRep ty (typeRep @Int) = readMaybe s
  | Just HRefl <- eqTypeRep ty (typeRep @String) = readMaybe s
  | otherwise = Nothing

data Length :: [a] -> Nat -> Type where
  LZero :: Length '[] 'Nat.Zero
  LSuc :: Length xs n -> Length (x ': xs) ('Nat.Suc n)

parseMany :: Length ts n -> STypes ts -> Vector n String -> Maybe (HVector ts)
parseMany prf tys ss =
  case tys of
    Types.SNil -> pure HVector.nil
    Types.SCons ty tys' ->
      case prf of
        LSuc prf' ->
          case Vector.match ss of
            Vector.MCons s ss' -> do
              s' <- parseOne ty s
              rest <- parseMany prf' tys' ss'
              pure $ HVector.cons s' rest

main :: IO ()
main = do
  countStr <- getLine
  Exists (n :: Strict.SNat n) <- maybe (error "not a number") pure $ parseSNat countStr
  tys :: Vector n String <- Vector.fromList n <$> List.replicateA (Strict.unroll n) getLine
  TysResult (tys' :: STypes tys) (prf :: Length tys n) (Dict :: Dict (All Show tys)) <-
    maybe (error "invalid type") pure $ parseTys tys
  vals :: Vector n String <- Vector.fromList n <$> List.replicateA (Strict.unroll n) getLine
  vals' <- maybe (error "invalid value") pure $ parseMany prf tys' vals
  print (HVector.toHList vals')
