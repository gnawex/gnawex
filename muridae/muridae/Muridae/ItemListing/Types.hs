module Muridae.ItemListing.Types
  ( ItemListing (..)
  , ItemSellListing
  , ItemBuyListing
  , ItemListingId (..)
  , ListingType (..)
  , PooledBuyListing (..)
  , PooledSellListing (..)
  , PrimaryKey (..)
  , FilterItemListingType (..)
  , mkItemSellListing
  , mkItemBuyListing
  , unItemSellListing
  , unItemBuyListing
  , mkPooledBuyListing
  , mkPooledSellListing
  )
where

import Data.Coerce (coerce)
import Data.Functor.Identity (Identity)
import Data.Int (Int16, Int32)
import Data.Kind (Type)
import Data.Time (UTCTime)
import Database.Beam
  ( Beamable
  , Columnar
  , FromBackendRow
  , HasSqlEqualityCheck
  , Table (PrimaryKey)
  , primaryKey
  )
import Database.Beam.Backend (HasSqlValueSyntax (sqlValueSyntax))
import Database.Beam.Postgres
  ( Postgres
  , ResultError (ConversionFailed, Incompatible, UnexpectedNull)
  )
import Database.Beam.Postgres.CustomTypes (pgEnumValueSyntax)
import Database.Beam.Postgres.Syntax (PgValueSyntax)
import Database.PostgreSQL.Simple.FromField
  ( FromField (fromField)
  , returnError
  , typename
  )
import GHC.Generics (Generic)
import Muridae.Item.Types (Item)
import Muridae.User.Types (User)

-------------------------------------------------------------------------------
-- Types

data ItemListing f = ItemListing
  { _id :: Columnar f ItemListingId
  , _tradable_item :: PrimaryKey Item f
  , _user :: PrimaryKey User f
  , _type :: Columnar f ListingType
  , _batched_by :: Columnar f Int16
  , _unit_quantity :: Columnar f Int32
  , _current_unit_quantity :: Columnar f Int32
  , _cost :: Columnar f Int32
  , _active :: Columnar f Bool
  , _created_at :: Columnar f UTCTime
  , _updated_at :: Columnar f (Maybe UTCTime)
  }
  deriving stock (Generic)
  deriving anyclass (Beamable)

newtype ItemSellListing f = ItemSellListing (ItemListing f)

newtype ItemBuyListing f = ItemBuyListing (ItemListing f)

newtype ItemListingId = ItemListingId Int32
  deriving stock (Generic, Show)
  deriving
    ( FromBackendRow Postgres
    , HasSqlEqualityCheck Postgres
    , HasSqlValueSyntax PgValueSyntax
    )
    via Int32

data ListingType = Buy | Sell
  deriving stock (Eq, Generic, Show)
  deriving anyclass (HasSqlEqualityCheck Postgres)

data PooledBuyListing = PooledBuyListing
  { cost :: Int32
  , batchedBy :: Int16
  , unitQuantity :: Int32
  }

data PooledSellListing = PooledSellListing
  { cost :: Int32
  , batchedBy :: Int16
  , unitQuantity :: Int32
  }

-- | Valid states when filtering item listings given its type
data FilterItemListingType
  = ByBuy
  -- ^ Filters out sell item listings, keeping only buy listings
  | BySell
  -- ^ Filters out buy item listings, keeping only sell listings
  | ByBoth
  -- ^ Filters in both buy and sell item listings

-------------------------------------------------------------------------------
-- Instances

deriving instance Show (ItemListing Identity)

deriving instance Show (PrimaryKey ItemListing Identity)

instance Table ItemListing where
  newtype PrimaryKey ItemListing f
    = ItemListingPk (Columnar f ItemListingId)
    deriving stock (Generic)

  primaryKey
    :: forall (column :: Type -> Type)
     . ItemListing column
    -> PrimaryKey ItemListing column
  primaryKey listing = ItemListingPk (listing._id)

instance Beamable (PrimaryKey ItemListing)

instance FromBackendRow Postgres ListingType

instance FromField ListingType where
  fromField f mbValue = do
    fieldType <- typename f
    case fieldType of
      "listing_type" -> do
        case mbValue of
          Nothing ->
            returnError UnexpectedNull f ""
          Just value ->
            case value of
              "buy" ->
                pure Buy
              "sell" ->
                pure Sell
              _ ->
                returnError
                  ConversionFailed
                  f
                  "Could not 'read' value for 'ListingType'"
      _ ->
        returnError Incompatible f ""

instance HasSqlValueSyntax PgValueSyntax ListingType where
  sqlValueSyntax = pgEnumValueSyntax $ \case
    Buy ->
      "buy"
    Sell ->
      "sell"

-------------------------------------------------------------------------------

mkItemSellListing :: ItemListing Identity -> Maybe (ItemSellListing Identity)
mkItemSellListing listing =
  \case
    Sell -> pure (ItemSellListing listing)
    Buy -> Nothing
    $ listing._type

mkItemBuyListing :: ItemListing Identity -> Maybe (ItemBuyListing Identity)
mkItemBuyListing listing =
  \case
    Buy -> pure (ItemBuyListing listing)
    Sell -> Nothing
    $ listing._type

unItemSellListing :: forall (f :: Type -> Type). ItemSellListing f -> ItemListing f
unItemSellListing = coerce

unItemBuyListing :: forall (f :: Type -> Type). ItemBuyListing f -> ItemListing f
unItemBuyListing = coerce

mkPooledBuyListing
  :: (ListingType, Int32, Int16, Int32) -> Maybe PooledBuyListing
mkPooledBuyListing (type_, cost, batchedBy, unitQuantity)
  | and [type_ == Buy, cost > 0, batchedBy > 0, unitQuantity > 0] =
      Just (PooledBuyListing cost batchedBy unitQuantity)
  | otherwise = Nothing

mkPooledSellListing
  :: (ListingType, Int32, Int16, Int32) -> Maybe PooledSellListing
mkPooledSellListing (type_, cost, batchedBy, unitQuantity)
  | and [type_ == Sell, cost > 0, batchedBy > 0, unitQuantity > 0] =
      Just (PooledSellListing cost batchedBy unitQuantity)
  | otherwise = Nothing
