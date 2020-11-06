module Sql.Query
  ( FieldNames(..)
  , Into(..)
  , InsertStyle(..)
  , Values(..)
  , insert
  , SelectStyle(..)
  , From(..)
  , select
  , Where(..)
  , selectW
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import Data.Foldable (fold)
import Data.Kind (Constraint, Type)
import qualified Data.List as List
import Data.Record (Record)
import qualified Data.Record as Record
import Data.Singletons.String (SString(..))
import qualified Data.Singletons.String as SString
import Data.Traversable (for)
import qualified Database.PostgreSQL.LibPQ as Postgres
import GHC.TypeLits (Symbol, KnownSymbol)

import Sql.Expr (Expr, renderExpr)
import Sql.Parser (parseRow)
import Sql.Schema (Schema, HasTable, getColumns)
import Sql.Schema.Column (Columns(..), ColumnTypes, ColumnInfo(..), Lookup(..))
import Sql.Schema.Column.Type (ColumnType, Nullable, KnownColumnType, renderValue)
import Sql.Schema.Constraint (ConstrTag(..), Constrs, constrsColumnType)
import Sql.Schema.Table (TableSpecColumns)

data SelectStyle :: [(Symbol, Type, [ConstrTag])] -> [(Symbol, Type, [ConstrTag])] -> Type where
  Star :: SelectStyle a a
  Tuple ::
    forall fieldNames some all.
    ContainsOut all fieldNames some =>
    FieldNames fieldNames ->
    SelectStyle all some

newtype From :: Symbol -> Type where
  From :: SString s -> From s

data Where :: [(Symbol, Type, [ConstrTag])] -> (Type -> Type) -> Type where
  Where :: Expr fields Bool -> Where fields []

data MaybeWhere :: [(Symbol, Type, [ConstrTag])] -> (Type -> Type) -> Type where
  NoWhere :: MaybeWhere fields []
  YesWhere :: Where fields coll -> MaybeWhere fields coll

select_ ::
  forall tableName tables tableSpec fields coll.
  HasTable tableName tables tableSpec =>
  Postgres.Connection ->
  Schema tables ->
  SelectStyle (TableSpecColumns tableSpec) fields ->
  From tableName ->
  MaybeWhere fields coll ->
  IO (coll (Record (ColumnTypes fields)))
select_ conn schema style (From tableName) where_ = do
  where_' <-
    case where_ of
       NoWhere -> pure mempty
       YesWhere (Where expr) -> (" WHERE " <>) <$> renderExpr conn expr
  mResult <-
    Postgres.exec conn $
    "SELECT " <>
    (case style of
       Star -> "*"
       Tuple fieldNames ->
         Char8.pack (renderFieldNames fieldNames)
    ) <>
    " FROM " <> Char8.pack (SString.toString tableName) <>
    where_' <>
    ";"
  case mResult of
    Nothing -> error "a fatal postgres error occurred"
    Just result -> do
      status <- Postgres.resultStatus result
      case status of
        Postgres.TuplesOk ->
          case where_ of
            NoWhere -> getRows result
            YesWhere Where{} -> getRows result
        _ -> do
          msg <- fold <$> Postgres.resultErrorMessage result
          error $ "postgres: " <> Char8.unpack msg
  where
    getRows result = do
      Postgres.Row numRows <- Postgres.ntuples result
      for [0..numRows-1] $ \row -> do
        parseRow cols (Postgres.toRow row) result

    cols :: Columns fields
    cols =
      let
        initialCols = getColumns @tableName @tables @tableSpec schema
      in
        case style of
          Star ->
            initialCols
          Tuple (_ :: FieldNames fieldNames) ->
            subColumnsOut @(TableSpecColumns tableSpec) @fieldNames @fields initialCols

selectW ::
  forall tableName tables tableSpec fields coll.
  HasTable tableName tables tableSpec =>
  Postgres.Connection ->
  Schema tables ->
  SelectStyle (TableSpecColumns tableSpec) fields ->
  From tableName ->
  Where fields coll ->
  IO (coll (Record (ColumnTypes fields)))
selectW conn schema style from where_ =
  select_ conn schema style from (YesWhere where_)

select ::
  forall tableName tables tableSpec fields.
  HasTable tableName tables tableSpec =>
  Postgres.Connection ->
  Schema tables ->
  SelectStyle (TableSpecColumns tableSpec) fields ->
  From tableName ->
  IO [Record (ColumnTypes fields)]
select conn schema style from =
  select_ conn schema style from NoWhere

renderRow :: forall cols. Postgres.Connection -> Columns cols -> Record (ColumnTypes cols) -> IO ByteString
renderRow conn cols row =
  case cols of
    Empty -> mempty
    Column (_ :: SString colName) (colTy :: ColumnType ty) (constrs :: Constrs tags ty ty') cols' -> do
      let
        colTy' = constrsColumnType colTy constrs

        val :: ty'
        val = Record.get @colName row
      val' <- renderValue conn colTy' val
      row' <- renderRow conn cols' (Record.retract row)
      pure $
        val' <>
        (case cols' of
          Empty -> mempty
          Column{} -> ", "
        ) <>
        row'

class
  ContainsOut
    (all :: [(Symbol, Type, [ConstrTag])])
    (fieldNames :: [Symbol])
    (some :: [(Symbol, Type, [ConstrTag])]) |
  all fieldNames -> some
  where
    subColumnsOut :: Columns all -> Columns some

instance ContainsOut all '[] '[] where
  subColumnsOut _ = Empty

instance
  (KnownSymbol fieldName, Lookup fieldName all fieldTy fieldTags, ContainsOut all fieldNames rest) =>
  ContainsOut all (fieldName ': fieldNames) ('(fieldName, fieldTy, fieldTags) ': rest)
  where
    subColumnsOut cols =
      case lookupInfo @fieldName @all @fieldTy cols of
        ColumnInfo ty constrs ->
          Column
            (SString @fieldName)
            ty
            constrs
            (subColumnsOut @all @fieldNames @rest cols)

type family FilterIn (xs :: [(Symbol, Type, [ConstrTag])]) :: [(Symbol, Type, [ConstrTag])] where
  FilterIn '[] = '[]
  FilterIn ('(s, Nullable t, tags) ': rest) = FilterIn rest
  FilterIn ('(s, t, 'Default ': tags) ': rest) = FilterIn rest
  FilterIn ('(s, t, tag ': tags) ': rest) = FilterIn ('(s, t, tags) ': rest)
  FilterIn ('(s, t, tags) ': rest) = '(s, t, tags) ': FilterIn rest

class
  LookupRemoving
    (fieldName :: Symbol)
    (all :: [(Symbol, Type, [ConstrTag])])
    (ty :: Type)
    (tags :: [ConstrTag])
    (all' :: [(Symbol, Type, [ConstrTag])]) |
  fieldName all -> ty tags all'
  where
    lookupInfoRemoving :: Columns all -> (Columns all', ColumnInfo ty tags)

instance
  {-# overlapping #-}
  KnownColumnType ty =>
  LookupRemoving s ('(s, ty, tags) ': rest) ty tags rest
  where
    lookupInfoRemoving (Column _ ty constrs rest) = (rest, ColumnInfo ty constrs)

instance
  {-# overlappable #-}
  (LookupRemoving s rest ty tags rest', rest'' ~ ('(s', ty', tags') ': rest')) =>
  LookupRemoving s ('(s', ty', tags') ': rest) ty tags rest''
  where
    lookupInfoRemoving (Column n ty constrs rest) =
      let
        (rest', val) = lookupInfoRemoving @s @rest @ty @tags @rest' rest
      in
        (Column n ty constrs rest', val)

type family Skippable (fieldTy :: Type) (fieldTags :: [ConstrTag]) :: Constraint where
  Skippable (Nullable ty) fieldTags = ()
  Skippable fieldTy ('Default ': rest) = ()
  Skippable fieldTy (tag ': rest) = Skippable fieldTy rest

class
  ContainsIn
    (all :: [(Symbol, Type, [ConstrTag])])
    (fieldNames :: [Symbol])
    (some :: [(Symbol, Type, [ConstrTag])]) |
  all fieldNames -> some
  where
    subColumnsIn :: Columns all -> Columns some

instance FilterIn all ~ '[] => ContainsIn all '[] '[] where
  subColumnsIn _ = Empty

instance
  {-# overlapping #-}
  ( KnownSymbol fieldName
  , LookupRemoving fieldName all fieldTy fieldTags all'
  , ContainsIn all' fieldNames rest
  ) =>
  ContainsIn all (fieldName ': fieldNames) ('(fieldName, fieldTy, fieldTags) ': rest)
  where
    subColumnsIn cols =
      case lookupInfoRemoving @fieldName @all @fieldTy @fieldTags @all' cols of
        (cols', ColumnInfo ty constrs) ->
          Column
            (SString @fieldName)
            ty
            constrs
            (subColumnsIn @all' @fieldNames @rest cols')

instance
  {-# overlappable #-}
  ( rest ~ (x ': xs)
  , Skippable fieldTy fieldTags
  , LookupRemoving fieldName all fieldTy fieldTags all'
  , ContainsIn all' fieldNames rest
  ) =>
  ContainsIn all (fieldName ': fieldNames) rest
  where
    subColumnsIn cols =
      let
        (cols', _) = lookupInfoRemoving @fieldName @all cols
      in
        subColumnsIn @all' @fieldNames @rest cols'

data FieldNames :: [Symbol] -> Type where
  FNil :: FieldNames '[]
  FCons :: SString s -> FieldNames ss -> FieldNames (s ': ss)

renderFieldNames :: FieldNames s -> String
renderFieldNames fns =
  case fns of
    FNil -> mempty
    FCons s rest ->
      SString.toString s <>
      (case rest of
         FNil -> mempty
         FCons{} -> ", "
      ) <>
      renderFieldNames rest

data InsertStyle :: [(Symbol, Type, [ConstrTag])] -> [(Symbol, Type, [ConstrTag])] -> Type where
  All :: InsertStyle a a
  Some ::
    forall fieldNames some all.
    ContainsIn all fieldNames some =>
    FieldNames fieldNames ->
    InsertStyle all some

newtype Values :: [(Symbol, Type)] -> Type where
  Values :: [Record fields] -> Values fields

newtype Into :: Symbol -> Type where
  Into :: SString s -> Into s

insert ::
  forall tableName tables tableSpec fields.
  HasTable tableName tables tableSpec =>
  Postgres.Connection ->
  Schema tables ->
  Into tableName ->
  InsertStyle (TableSpecColumns tableSpec) fields ->
  Values (ColumnTypes fields) ->
  IO ()
insert conn schema (Into tableName) style (Values rows) = do
  rows' <-
    fold . List.intersperse ", " <$>
    traverse (fmap (("(" <>) . (<> ")")) . renderRow conn cols) rows
  mResult <-
    Postgres.exec conn $
    "INSERT INTO " <> Char8.pack (SString.toString tableName) <>
    (case style of
       All -> mempty
       Some fieldNames -> "(" <> Char8.pack (renderFieldNames fieldNames) <> ")"
    ) <>
    " VALUES " <> rows' <> ";"
  case mResult of
    Nothing -> error "a fatal postgres error occurred"
    Just result -> do
      status <- Postgres.resultStatus result
      case status of
        Postgres.CommandOk -> pure ()
        _ -> do
          msg <- fold <$> Postgres.resultErrorMessage result
          error $ "postgres: " <> Char8.unpack msg
  where
    cols :: Columns fields
    cols =
      let
        initialCols = getColumns @tableName @tables @tableSpec schema
      in
        case style of
          All ->
            initialCols
          Some (_ :: FieldNames fieldNames) ->
            subColumnsIn @(TableSpecColumns tableSpec) @fieldNames @fields initialCols
