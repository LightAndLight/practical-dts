module Sql.Schema.Column
  ( Columns(..)
  , renderColumns
  , Lookup(..)
  , ColumnTypes
  , ColumnInfo(..)
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import Data.Kind (Type)
import Data.Singletons.String (SString(..))
import qualified Data.Singletons.String as SString
import qualified Database.PostgreSQL.LibPQ as Postgres
import GHC.TypeLits (Symbol)

import Sql.Schema.Constraint (ConstrTag, Constrs, renderConstrs)
import Sql.Schema.Column.Type (ColumnType(..), KnownColumnType)

data Columns :: [(Symbol, Type, [ConstrTag])] -> Type where
  Empty :: Columns '[]
  Column ::
    SString s ->
    ColumnType a ->
    Constrs tags a b ->
    Columns cols ->
    Columns ('(s, b, tags) ': cols)

renderColumnType :: ColumnType a -> String
renderColumnType = go False
  where
    go :: Bool -> ColumnType a -> String
    go nullable colType =
      case colType of
        TString -> "TEXT" <> nullTag nullable
        TBool -> "BOOLEAN" <> nullTag nullable
        TInteger -> "INTEGER" <> nullTag nullable
        TBinary -> "BLOB" <> nullTag nullable
        TNullable ty -> go True ty

    nullTag nullable = if nullable then "" else " NOT NULL"

renderColumns :: Postgres.Connection -> Columns cols -> IO ByteString
renderColumns conn cols =
  case cols of
    Empty ->
      pure mempty
    Column colName colTy constrs rest -> do
      constrs' <- renderConstrs conn colTy constrs
      rest' <- renderColumns conn rest
      pure $
        Char8.pack (SString.toString colName) <> " " <> Char8.pack (renderColumnType colTy) <> constrs' <>
        (case rest of
          Empty -> mempty
          Column{} -> ","
        ) <>
        "\n" <>
        rest'

type family ColumnTypes (cs :: [(Symbol, Type, [ConstrTag])]) :: [(Symbol, Type)] where
  ColumnTypes '[] = '[]
  ColumnTypes ('(n, ty, _) ': rest) = '(n, ty) ': ColumnTypes rest

data ColumnInfo :: Type -> [ConstrTag] -> Type where
  ColumnInfo :: ColumnType ty -> Constrs tags ty ty' -> ColumnInfo ty' tags

class
  Lookup (fieldName :: Symbol) (all :: [(Symbol, Type, [ConstrTag])]) (ty :: Type) (tags :: [ConstrTag]) |
  fieldName all -> ty tags
  where
    lookupInfo :: Columns all -> ColumnInfo ty tags

instance {-# overlapping #-} KnownColumnType ty => Lookup s ('(s, ty, tags) ': rest) ty tags where
  lookupInfo (Column _ ty constrs _) = ColumnInfo ty constrs

instance {-# overlappable #-} Lookup s rest ty tags => Lookup s ('(s', ty', tags') ': rest) ty tags where
  lookupInfo (Column _ _ _ rest) = lookupInfo @s @rest @ty @tags rest
