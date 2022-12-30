{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module DB.Types where

import Data.Functor.Identity (Identity)
import Data.Int (Int16, Int32, Int64)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Beam (Database, FromBackendRow, TableEntity)
import Database.Beam.Backend.SQL (HasSqlValueSyntax, sqlValueSyntax)
import Database.Beam.Postgres (Postgres, ResultError (ConversionFailed, Incompatible, UnexpectedNull))
import Database.Beam.Postgres.CustomTypes (pgEnumValueSyntax)
import Database.Beam.Postgres.Syntax (PgValueSyntax)
import Database.Beam.Schema (Beamable, Columnar, Table (PrimaryKey, primaryKey))
import Database.PostgreSQL.Simple.FromField (FromField, fromField, returnError, typename)
import GHC.Generics (Generic)

-------------------------------------------------------------------------------
-- DB

data MuridaeDB f = MuridaeDB
  { muridaeTradableItems :: f (TableEntity TradableItem)
  , muridaeTradableItemListings :: f (TableEntity TradableItemListing)
  }
  deriving stock (Generic)
  deriving anyclass (Database Postgres)

-------------------------------------------------------------------------------
-- `apps.tradable_items`

data TradableItem f = TradableItem
  { _id :: Columnar f TradableItemId
  , _name :: Columnar f Text
  , _description :: Columnar f Text
  , _wiki_link :: Columnar f Text
  , _created_at :: Columnar f UTCTime
  , _updated_at :: Columnar f (Maybe UTCTime)
  , _deleted_at :: Columnar f (Maybe UTCTime)
  }
  deriving stock (Generic)
  deriving anyclass (Beamable)

newtype TradableItemId = TradableItemId Int32
  deriving stock (Generic, Show)
  deriving (FromBackendRow Postgres) via Int32

deriving instance Show (TradableItem Identity)

instance Table TradableItem where
  newtype PrimaryKey TradableItem f
    = TradableItemIdPk (Columnar f TradableItemId)
    deriving stock (Generic)

  primaryKey ::
    forall (column :: Type -> Type).
    TradableItem column ->
    PrimaryKey TradableItem column
  primaryKey item = TradableItemIdPk (item._id)

instance Beamable (PrimaryKey TradableItem)

-------------------------------------------------------------------------------
-- `apps.tradable_item_listings`

data ListingType = Buy | Sell
  deriving stock (Generic, Show)

data TradableItemListing f = TradableItemListing
  { _id :: Columnar f TradableItemListingId
  , _tradable_item :: PrimaryKey TradableItem f
  , _type :: Columnar f ListingType
  , _batched_by :: Columnar f Int16
  , _unit_quantity :: Columnar f Int32
  , _cost :: Columnar f Int64
  , _active :: Columnar f Bool
  , _created_at :: Columnar f UTCTime
  , _updated_at :: Columnar f (Maybe UTCTime)
  }
  deriving stock (Generic)
  deriving anyclass (Beamable)

deriving instance Show (TradableItemListing Identity)
deriving instance Show (PrimaryKey TradableItem Identity)

newtype TradableItemListingId = TradableItemListingId Int32
  deriving stock (Generic, Show)
  deriving (FromBackendRow Postgres) via Int32

instance Table TradableItemListing where
  newtype PrimaryKey TradableItemListing f
    = TradableItemListingIdPk (Columnar f TradableItemListingId)
    deriving stock (Generic)

  primaryKey ::
    forall (column :: Type -> Type).
    TradableItemListing column ->
    PrimaryKey TradableItemListing column
  primaryKey listing = TradableItemListingIdPk (listing._id)

instance Beamable (PrimaryKey TradableItemListing)

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
                returnError ConversionFailed f "Could not 'read' value for 'ListingType'"
      _ ->
        returnError Incompatible f ""

instance HasSqlValueSyntax PgValueSyntax ListingType where
  sqlValueSyntax = pgEnumValueSyntax $ \case
    Buy ->
      "buy"
    Sell ->
      "sell"
