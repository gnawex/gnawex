module Muridae.Item.Types where

import Data.Functor.Identity (Identity)
import Data.Int (Int32)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Beam (
    Beamable,
    Columnar,
    FromBackendRow,
    HasSqlEqualityCheck,
    Table (PrimaryKey, primaryKey),
 )
import Database.Beam.Backend (HasSqlValueSyntax)
import Database.Beam.Postgres (Postgres)
import Database.Beam.Postgres.Syntax (PgValueSyntax)
import GHC.Generics (Generic)

-------------------------------------------------------------------------------
-- Types

data Item f = Item
    { _id :: Columnar f ItemId
    , _name :: Columnar f Text
    , _description :: Columnar f Text
    , _wiki_link :: Columnar f Text
    , _created_at :: Columnar f UTCTime
    , _updated_at :: Columnar f (Maybe UTCTime)
    , _deleted_at :: Columnar f (Maybe UTCTime)
    }
    deriving stock (Generic)
    deriving anyclass (Beamable)

newtype ItemId = ItemId Int32
    deriving stock (Generic, Show)
    deriving
        ( FromBackendRow Postgres
        , HasSqlValueSyntax PgValueSyntax
        , HasSqlEqualityCheck Postgres
        )
        via Int32

-------------------------------------------------------------------------------
-- Instances

deriving instance Show (Item Identity)

deriving instance Show (PrimaryKey Item Identity)

instance Table Item where
    newtype PrimaryKey Item f
        = ItemPk (Columnar f ItemId)
        deriving stock (Generic)

    primaryKey
        :: forall (column :: Type -> Type)
         . Item column
        -> PrimaryKey Item column
    primaryKey item = ItemPk (item._id)

instance Beamable (PrimaryKey Item)
