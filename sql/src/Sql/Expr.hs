module Sql.Expr
  ( Expr(..)
  , renderExpr
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import Data.Kind (Type)
import Data.Singletons.String (SString(..))
import qualified Data.Singletons.String as SString
import qualified Database.PostgreSQL.LibPQ as Postgres
import GHC.OverloadedLabels (IsLabel(..))
import GHC.TypeLits (Symbol, KnownSymbol)

import Sql.Schema.Constraint (ConstrTag)
import Sql.Schema.Column (Lookup)
import Sql.Schema.Column.Type (KnownColumnType(..), renderValue)

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
