module Sql.Schema.Constraint
  ( ConstrTag(..)
  , Constr(..)
  , renderConstr
  , constrColumnType
  , Constrs(..)
  , renderConstrs
  , constrsColumnType
  )
where

import Data.ByteString (ByteString)
import Data.Kind (Type)
import qualified Database.PostgreSQL.LibPQ as Postgres

import Sql.Schema.Column.Type (ColumnType, renderValue)

data Constr :: ConstrTag -> Type -> Type -> Type where
  CPrimaryKey :: Constr 'PrimaryKey a a
  CDefault :: a -> Constr 'Default a a
  CUnique :: Constr 'Unique a a
deriving instance Show a => Show (Constr tags a b)

data Constrs :: [ConstrTag] -> Type -> Type -> Type where
  CNone :: Constrs '[] a a
  CCons :: Constr tag a b -> Constrs tags b c -> Constrs (tag ': tags) a c

data ConstrTag = Unique | Default | PrimaryKey

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
      rest' <- renderConstrs conn (constrColumnType ty constr) rest
      pure $
        " " <>
        constr' <>
        rest'

constrColumnType :: ColumnType ty -> Constr tag ty ty' -> ColumnType ty'
constrColumnType ty constr =
  case constr of
    CPrimaryKey -> ty
    CDefault{} -> ty
    CUnique{} -> ty

constrsColumnType :: ColumnType ty -> Constrs tags ty ty' -> ColumnType ty'
constrsColumnType ty constrs =
  case constrs of
    CNone -> ty
    CCons constr rest ->
      let
        ty' = constrColumnType ty constr
      in
        constrsColumnType ty' rest
