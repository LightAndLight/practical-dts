module Sql.Schema.Table
  ( TableSpec(..)
  , TableSpecColumns
  , renderCreateTable
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import Data.Kind (Type)
import Data.Singletons.String (SString)
import qualified Data.Singletons.String as SString
import qualified Database.PostgreSQL.LibPQ as Postgres
import GHC.TypeLits (Symbol)

import Sql.Schema.Constraint (ConstrTag)
import Sql.Schema.Column (Columns, renderColumns)

newtype TableSpec = TableSpec [(Symbol, Type, [ConstrTag])]

type family TableSpecColumns (tableSpec :: TableSpec) :: [(Symbol, Type, [ConstrTag])] where
  TableSpecColumns ('TableSpec cols) = cols

renderCreateTable :: Postgres.Connection -> SString name -> Columns cols -> IO ByteString
renderCreateTable conn name cols = do
  cols' <- renderColumns conn cols
  pure $
    "CREATE TABLE " <> Char8.pack (SString.toString name) <> " (" <>
    cols' <>
    ");"
