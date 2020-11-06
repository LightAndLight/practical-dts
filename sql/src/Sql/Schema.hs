module Sql.Schema
  ( Schema(..)
  , HasTable(..)
  , renderSchema
  , createSchema
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import Data.Foldable (fold)
import Data.Kind (Type)
import Data.Singletons.String (SString)
import qualified Database.PostgreSQL.LibPQ as Postgres
import GHC.TypeLits (Symbol)

import Sql.Schema.Column (Columns)
import Sql.Schema.Table (TableSpec(..), TableSpecColumns, renderCreateTable)

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
