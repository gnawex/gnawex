module Muridae.ItemTxn.Types where

import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.Kind (Type)
import Data.String (IsString)
import Data.Time (UTCTime)
import Data.UUID.Types (UUID)
import Database.Beam (
  Beamable,
  Columnar,
  FromBackendRow,
  Generic,
  Identity,
  Table (PrimaryKey, primaryKey),
 )
import Database.Beam.Backend (HasSqlValueSyntax (sqlValueSyntax))
import Database.Beam.Postgres (Postgres, ResultError (Incompatible, UnexpectedNull))
import Database.Beam.Postgres.CustomTypes (pgEnumValueSyntax)
import Database.Beam.Postgres.Syntax (PgValueSyntax)
import Database.PostgreSQL.Simple.FromField (
  Conversion,
  Field,
  FromField (fromField),
  returnError,
  typename,
 )
import Muridae.ItemListing.Types (ItemListing)
import Muridae.User.Types (User)

-------------------------------------------------------------------------------
-- Types

data ItemTxn f = ItemTxn
  { _id :: Columnar f ItemTxnId
  -- ^ Transaction ID
  , _buy_item_listing :: PrimaryKey ItemListing f
  -- ^ The buy item listing that was matched
  , _sell_item_listing :: PrimaryKey ItemListing f
  -- ^ The sell item listing that was matched
  , _quantity :: Columnar f Int32
  -- ^ The amount of a unit quantity that is being/to be transacted
  , _status :: Columnar f Status
  -- ^ Transaction status
  , _buyer :: PrimaryKey User f
  , _seller :: PrimaryKey User f
  , _created_at :: Columnar f UTCTime
  , _updated_at :: Columnar f (Maybe UTCTime)
  }
  deriving stock (Generic)
  deriving anyclass (Beamable)

newtype ItemTxnId = ItemTxnId UUID
  deriving stock (Generic, Show)
  deriving (FromBackendRow Postgres) via UUID

data Status = Pending | Completed | Cancelled
  deriving stock (Generic, Show)

-------------------------------------------------------------------------------
-- Instances

deriving instance Show (ItemTxn Identity)
deriving instance Show (PrimaryKey ItemTxn Identity)

instance Table ItemTxn where
  newtype PrimaryKey ItemTxn f = ItemTxnPk (Columnar f ItemTxnId)
    deriving stock (Generic)

  primaryKey :: forall (col :: Type -> Type). ItemTxn col -> PrimaryKey ItemTxn col
  primaryKey txn = ItemTxnPk (txn._id)

instance Beamable (PrimaryKey ItemTxn)
instance FromBackendRow Postgres Status

instance FromField Status where
  fromField :: Field -> Maybe ByteString -> Conversion Status
  fromField f val = do
    fieldType <- typename f
    case fieldType of
      "transaction_status" -> do
        case strToStatus =<< val of
          Just val' -> pure val'
          Nothing -> returnError UnexpectedNull f ""
      _ -> returnError Incompatible f ""

instance HasSqlValueSyntax PgValueSyntax Status where
  sqlValueSyntax :: Status -> PgValueSyntax
  sqlValueSyntax = pgEnumValueSyntax statusToStr

------------------------------------------------------------------------------

strToStatus :: forall (s :: Type). (Eq s, IsString s) => s -> Maybe Status
strToStatus = \case
  "pending" -> pure Pending
  "completed" -> pure Completed
  "cancelled" -> pure Cancelled
  _ -> Nothing

statusToStr :: forall (s :: Type). (Eq s, IsString s) => Status -> s
statusToStr = \case
  Pending -> "pending"
  Completed -> "completed"
  Cancelled -> "cancelled"
