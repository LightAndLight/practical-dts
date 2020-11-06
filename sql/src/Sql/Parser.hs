module Sql.Parser
  ( ParseError(..)
  , parseValue
  , parseRow
  )
where

import Control.Applicative ((<|>))
import Control.Exception (Exception, throw)
import qualified Data.Attoparsec.ByteString.Char8 as Attoparsec
import Data.ByteString (ByteString)
import Data.Record (Record)
import qualified Data.Record as Record
import Data.Singletons.String (SString)
import qualified Data.Text.Encoding as Encoding
import Data.Typeable (Typeable)
import qualified Database.PostgreSQL.LibPQ as Postgres

import Sql.Schema.Constraint (Constrs, constrsColumnType)
import Sql.Schema.Column (Columns(..), ColumnTypes)
import Sql.Schema.Column.Type (ColumnType(..), Nullable(..))

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

parseRow :: Columns cols -> Postgres.Row -> Postgres.Result -> IO (Record (ColumnTypes cols))
parseRow = go (Postgres.toColumn (0::Int))
  where
    go :: Postgres.Column -> Columns cols -> Postgres.Row -> Postgres.Result -> IO (Record (ColumnTypes cols))
    go colNumber cols rowNumber result =
      case cols of
        Empty -> pure Record.empty
        Column (colName :: SString colName) (colTy :: ColumnType ty) (constrs :: Constrs tags ty ty') cols' -> do
          let colTy' = constrsColumnType colTy constrs
          mVal <- Postgres.getvalue' result rowNumber colNumber
          val <-
            case mVal of
              Nothing ->
                case colTy' of
                  TNullable{} -> pure Null
                  _ -> error $ "failed to get value for: " <> show colName <> ", " <> show colTy'
              Just val -> either throw pure (parseValue colTy' val)
          Record.extend @colName val <$> go (colNumber + 1) cols' rowNumber result
