{-# language DataKinds #-}
{-# language OverloadedLabels #-}
{-# language OverloadedStrings #-}
{-# language TypeApplications #-}
module Main where

import Control.Exception (bracket)
import qualified Data.Record as Record
import qualified Database.PostgreSQL.LibPQ as Postgres

import Sql.Expr (Expr(..))
import Sql.Query
  ( FieldNames(..), From(..), InsertStyle(..), Into(..), SelectStyle(..), Where(..), Values(..)
  , insert, select, selectW
  )
import Sql.Schema.Column.Type (ColumnType(..))
import Sql.Schema.Column (Columns(..))
import Sql.Schema.Constraint (Constr(..), Constrs(..))
import Sql.Schema (Schema(..), createSchema)


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
