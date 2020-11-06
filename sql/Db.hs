{-# language AllowAmbiguousTypes #-}
{-# language DataKinds, GADTs, KindSignatures #-}
{-# language FlexibleInstances, UndecidableInstances #-}
{-# language FunctionalDependencies, MultiParamTypeClasses #-}
{-# language OverloadedStrings #-}
{-# language OverloadedLabels #-}
{-# language ScopedTypeVariables, TypeApplications #-}
{-# language StandaloneDeriving #-}
{-# language TypeOperators #-}
{-# language TypeFamilies #-}
module Db
  ( Schema(..)
  , renderSchema
  , Columns(..)
  , ColumnType(..)
  , ParseError(..)
  , HasTable
  , getTableName
  , getColumns
  , select
  , selectW
  , insert
  , createSchema
  , main
  )
where

import Control.Applicative ((<|>))
import Control.Exception (Exception, bracket, throw)
import qualified Data.Attoparsec.ByteString.Char8 as Attoparsec
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import Data.Foldable (fold)
import qualified Data.List as List
import Data.Traversable (for)
import Data.Kind (Constraint, Type)
import Data.Record (Record)
import qualified Data.Record as Record
import Data.Singletons.String (SString(..))
import qualified Data.Singletons.String as SString
import Data.Text (Text)
import qualified Data.Text.Encoding as Encoding
import Data.Typeable (Typeable)
import qualified Database.PostgreSQL.LibPQ as Postgres
import GHC.OverloadedLabels (IsLabel(..))
import GHC.TypeLits (Symbol, KnownSymbol)

data Nullable a = Null | NotNull a

data ColumnType :: Type -> Type where
  TString :: ColumnType Text
  TBool :: ColumnType Bool
  TInteger :: ColumnType Int
  TBinary :: ColumnType ByteString
  TNullable :: ColumnType ty -> ColumnType (Nullable ty)
deriving instance Show (ColumnType ty)

class KnownColumnType ty where; columnTypeVal :: ColumnType ty
instance KnownColumnType Text where; columnTypeVal = TString
instance KnownColumnType Bool where; columnTypeVal = TBool
instance KnownColumnType Int where; columnTypeVal = TInteger
instance KnownColumnType ByteString where; columnTypeVal = TBinary
instance KnownColumnType ty => KnownColumnType (Nullable ty) where; columnTypeVal = TNullable (columnTypeVal @ty)

data Constr :: ConstrTag -> Type -> Type -> Type where
  CPrimaryKey :: Constr 'PrimaryKey a a
  CDefault :: a -> Constr 'Default a a
  CUnique :: Constr 'Unique a a
deriving instance Show a => Show (Constr tags a b)

data Constrs :: [ConstrTag] -> Type -> Type -> Type where
  CNone :: Constrs '[] a a
  CCons :: Constr tag a b -> Constrs tags b c -> Constrs (tag ': tags) a c

data ConstrTag = Unique | Default | PrimaryKey

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

renderConstr :: Postgres.Connection -> ColumnType a -> Constr tag a b -> IO ByteString
renderConstr conn ty constr =
  case constr of
    CPrimaryKey -> pure "PRIMARY KEY"
    CDefault val -> ("DEFAULT " <>) <$> renderValue conn ty val
    CUnique -> pure "UNIQUE"

renderConstrs :: Postgres.Connection -> ColumnType a -> Constrs tags a b -> IO ByteString
renderConstrs conn ty constrs =
  case constrs of
    CNone -> pure mempty
    CCons constr rest -> do
      constr' <- renderConstr conn ty constr
      rest' <- renderConstrs conn (constrainColumnType' ty constr) rest
      pure $
        " " <>
        constr' <>
        rest'

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

renderCreateTable :: Postgres.Connection -> SString name -> Columns cols -> IO ByteString
renderCreateTable conn name cols = do
  cols' <- renderColumns conn cols
  pure $
    "CREATE TABLE " <> Char8.pack (SString.toString name) <> " (" <>
    cols' <>
    ");"

newtype TableSpec = TableSpec [(Symbol, Type, [ConstrTag])]

type family TableSpecColumns (tableSpec :: TableSpec) :: [(Symbol, Type, [ConstrTag])] where
  TableSpecColumns ('TableSpec cols) = cols

data Schema :: [(Symbol, TableSpec)] -> Type where
  Done :: Schema '[]
  Table ::
    SString name ->
    Columns cols ->
    Schema tables ->
    Schema ('(name, 'TableSpec cols) ': tables)

renderSchema :: Postgres.Connection -> Schema tables -> IO ByteString
renderSchema conn schema =
  case schema of
    Done -> mempty
    Table name cols rest -> do
      cols' <- renderCreateTable conn name cols
      rest' <- renderSchema conn rest
      pure $
        cols' <>
        (case rest of
          Done -> mempty
          Table{} -> "\n") <>
        rest'

class
  HasTable (tableName :: Symbol) (tables :: [(Symbol, TableSpec)]) (tableSpec :: TableSpec) |
  tableName tables -> tableSpec
  where
    getTableName :: Schema tables -> SString tableName
    getColumns :: Schema tables -> Columns (TableSpecColumns tableSpec)

instance
  {-# overlappable #-} HasTable tableName rest tableSpec =>
  HasTable tableName ('(tableName', tableSpec') ': rest) tableSpec
  where
    getTableName (Table _ _ rest) = getTableName @tableName @rest @tableSpec rest
    getColumns (Table _ _ rest) = getColumns @tableName @rest @tableSpec rest

instance {-# overlapping #-} HasTable tableName ('(tableName, tableSpec) ': rest) tableSpec where
  getTableName (Table name _ _) = name
  getColumns (Table _ cols _) = cols

data ParseError where
  ParseError :: String -> ParseError
deriving instance Show ParseError
deriving instance Typeable ParseError

instance Exception ParseError

parseValue :: ColumnType ty -> ByteString -> Either ParseError ty
parseValue colTy val =
  case Attoparsec.parseOnly (parser colTy) val of
    Left err -> Left $ ParseError err
    Right res -> pure res
  where
    parser :: ColumnType ty -> Attoparsec.Parser ty
    parser ty =
      case ty of
        TString -> pure $ Encoding.decodeUtf8 val
        TBool -> boolParser
        TInteger -> integerParser
        TBinary -> pure val
        TNullable ty' -> nullableParser (parser ty')

    nullableParser :: Attoparsec.Parser a -> Attoparsec.Parser (Nullable a)
    nullableParser p = Null <$ Attoparsec.string "null" <|> NotNull <$> p

    boolParser :: Attoparsec.Parser Bool
    boolParser =
      True <$ Attoparsec.char 't' <|>
      False <$ Attoparsec.char 'f'

    integerParser :: Attoparsec.Parser Int
    integerParser = ($) <$> (negate <$ Attoparsec.char '-' <|> pure id) <*> Attoparsec.decimal <* Attoparsec.endOfInput

constrainColumnType' :: ColumnType ty -> Constr tag ty ty' -> ColumnType ty'
constrainColumnType' ty constr =
  case constr of
    CPrimaryKey -> ty
    CDefault{} -> ty
    CUnique{} -> ty

constrainColumnType :: ColumnType ty -> Constrs tags ty ty' -> ColumnType ty'
constrainColumnType ty constrs =
  case constrs of
    CNone -> ty
    CCons constr rest ->
      let
        ty' = constrainColumnType' ty constr
      in
        constrainColumnType ty' rest

type family ColumnTypes (cs :: [(Symbol, Type, [ConstrTag])]) :: [(Symbol, Type)] where
  ColumnTypes '[] = '[]
  ColumnTypes ('(n, ty, _) ': rest) = '(n, ty) ': ColumnTypes rest

parseRow :: Columns cols -> Postgres.Row -> Postgres.Result -> IO (Record (ColumnTypes cols))
parseRow = go (Postgres.toColumn (0::Int))
  where
    go :: Postgres.Column -> Columns cols -> Postgres.Row -> Postgres.Result -> IO (Record (ColumnTypes cols))
    go colNumber cols rowNumber result =
      case cols of
        Empty -> pure Record.empty
        Column (colName :: SString colName) (colTy :: ColumnType ty) (constrs :: Constrs tags ty ty') cols' -> do
          let colTy' = constrainColumnType colTy constrs
          mVal <- Postgres.getvalue' result rowNumber colNumber
          val <-
            case mVal of
              Nothing ->
                case colTy' of
                  TNullable{} -> pure Null
                  _ -> error $ "failed to get value for: " <> show colName <> ", " <> show colTy'
              Just val -> either throw pure (parseValue colTy' val)
          Record.extend @colName val <$> go (colNumber + 1) cols' rowNumber result

data SelectStyle :: [(Symbol, Type, [ConstrTag])] -> [(Symbol, Type, [ConstrTag])] -> Type where
  Star :: SelectStyle a a
  Tuple ::
    forall fieldNames some all.
    ContainsOut all fieldNames some =>
    FieldNames fieldNames ->
    SelectStyle all some

newtype From :: Symbol -> Type where
  From :: SString s -> From s

data Expr :: [(Symbol, Type, [ConstrTag])] -> Type -> Type where
  Var :: Lookup s ctx ty tags => SString s -> Expr ctx ty
  (:==) :: Expr ctx a -> Expr ctx a -> Expr ctx Bool
  Val :: KnownColumnType ty => ty -> Expr ctx ty

instance (KnownSymbol s, Lookup s ctx ty tags) => IsLabel s (Expr ctx ty) where
  fromLabel = Var (SString :: SString s)

renderExpr :: forall ctx ty. Postgres.Connection -> Expr ctx ty -> IO ByteString
renderExpr conn expr =
  case expr of
    Var n -> pure $ Char8.pack (SString.toString n)
    a :== b -> do
      a' <- renderExpr conn a
      b' <- renderExpr conn b
      pure $ a' <> " = " <> b'
    Val v -> renderValue conn (columnTypeVal @ty) v

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

renderValue :: Postgres.Connection -> ColumnType ty -> ty -> IO ByteString
renderValue conn colTy val =
  case colTy of
    TString -> do
      mStr <- Postgres.escapeStringConn conn (Encoding.encodeUtf8 val)
      case mStr of
        Nothing -> error "a fatal postgres error occurred"
        Just str -> pure $ "'" <> str <> "'"
    TBool -> pure $ if val then "TRUE" else "FALSE"
    TInteger -> pure $ Char8.pack (show val)
    TBinary -> pure val
    TNullable ty' ->
      case val of
        Null -> pure "null"
        NotNull val' -> renderValue conn ty' val'


renderRow :: forall cols. Postgres.Connection -> Columns cols -> Record (ColumnTypes cols) -> IO ByteString
renderRow conn cols row =
  case cols of
    Empty -> mempty
    Column (_ :: SString colName) (colTy :: ColumnType ty) (constrs :: Constrs tags ty ty') cols' -> do
      let
        colTy' = constrainColumnType colTy constrs

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
  Lookup (fieldName :: Symbol) (all :: [(Symbol, Type, [ConstrTag])]) (ty :: Type) (tags :: [ConstrTag]) |
  fieldName all -> ty tags
  where
    lookupInfo :: Columns all -> ColumnInfo ty tags

instance {-# overlapping #-} KnownColumnType ty => Lookup s ('(s, ty, tags) ': rest) ty tags where
  lookupInfo (Column _ ty constrs _) = ColumnInfo ty constrs

instance {-# overlappable #-} Lookup s rest ty tags => Lookup s ('(s', ty', tags') ': rest) ty tags where
  lookupInfo (Column _ _ _ rest) = lookupInfo @s @rest @ty @tags rest

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

class
  ContainsIn
    (all :: [(Symbol, Type, [ConstrTag])])
    (fieldNames :: [Symbol])
    (some :: [(Symbol, Type, [ConstrTag])]) |
  all fieldNames -> some
  where
    subColumnsIn :: Columns all -> Columns some

type family FilterIn (xs :: [(Symbol, Type, [ConstrTag])]) :: [(Symbol, Type, [ConstrTag])] where
  FilterIn '[] = '[]
  FilterIn ('(s, Nullable t, tags) ': rest) = FilterIn rest
  FilterIn ('(s, t, 'Default ': tags) ': rest) = FilterIn rest
  FilterIn ('(s, t, tag ': tags) ': rest) = FilterIn ('(s, t, tags) ': rest)
  FilterIn ('(s, t, tags) ': rest) = '(s, t, tags) ': FilterIn rest

instance FilterIn all ~ '[] => ContainsIn all '[] '[] where
  subColumnsIn _ = Empty

data ColumnInfo :: Type -> [ConstrTag] -> Type where
  ColumnInfo :: ColumnType ty -> Constrs tags ty ty' -> ColumnInfo ty' tags

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

type family Skippable (fieldTy :: Type) (fieldTags :: [ConstrTag]) :: Constraint where
  Skippable (Nullable ty) fieldTags = ()
  Skippable fieldTy ('Default ': rest) = ()
  Skippable fieldTy (tag ': rest) = Skippable fieldTy rest

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

createSchema :: Postgres.Connection -> Schema tables -> IO ()
createSchema conn schema = do
  schema' <- renderSchema conn schema
  mResult <- Postgres.exec conn schema'
  case mResult of
    Nothing -> error "a fatal postgres error occurred"
    Just result -> do
      status <- Postgres.resultStatus result
      case status of
        Postgres.CommandOk -> pure ()
        _ -> do
          msg <- fold <$> Postgres.resultErrorMessage result
          error $ "postgres: " <> Char8.unpack msg

main :: IO ()
main =
  bracket (Postgres.connectdb "host=localhost port=5432 dbname=testing") Postgres.finish $ \conn -> do
    createSchema conn schema

{-
    insert conn schema
      (Into #mytable)
      All
      (Values [Record.extend @"col1" True $ Record.extend @"col2" "no" $ Record.extend @"col3" Nothing Record.empty])
-}

    insert conn schema
      (Into #mytable)
      (Some $ FCons #col1 $ FCons #col2 $ FCons #col3 FNil)
      (Values [Record.extend @"col1" False $ Record.extend @"col2" "yes" $ Record.extend @"col3" 99 Record.empty])
    insert conn schema
      (Into #mytable)
      (Some $ FCons #col1 $ FCons #col2 FNil)
      (Values [Record.extend @"col1" True $ Record.extend @"col2" "maybe" Record.empty])

    {-
    with a quasi quoter for records:

    insert conn schema
      (Into #mytable)
      All
      (Values [ [r| col1 = False, col2 = "hello", col3 = Just 99 |] ])
    -}

    print =<< select conn schema Star (From #mytable)
    print =<< selectW conn schema Star (From #mytable) (Where $ #col3 :== Val 99)
  where
    schema =
      Table #mytable
        (Column #col1 TBool CNone $
         Column #col2 TString CNone $
         Column #col3 TInteger (CCons (CDefault 2) CNone) $
         Empty
        ) $
      Done
