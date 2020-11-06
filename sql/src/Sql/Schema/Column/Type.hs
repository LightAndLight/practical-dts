module Sql.Schema.Column.Type
  ( ColumnType(..)
  , Nullable(..)
  , KnownColumnType(..)
  , renderValue
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text.Encoding as Encoding
import qualified Database.PostgreSQL.LibPQ as Postgres

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
