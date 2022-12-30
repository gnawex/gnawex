module MuridaeWeb.Handler.Item.Listing.Types where

import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Int (Int16, Int32, Int64)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import MuridaeWeb.Handler.Item.Types (TradableItemId)
import MuridaeWeb.Handler.User (UserId)

newtype TradableItemListingId = TradableItemListingId Int32
  deriving (ToJSON) via Int32

data TradableItemListingType = BUY | SELL
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

data TradableItemListing = TradableItemListing
  { id :: TradableItemListingId
  , tradable_item_id :: TradableItemId
  , owner_id :: UserId
  , listing_type :: TradableItemListingType
  , batched_by :: Int16
  , unit_quantity :: Int32
  , cost :: Int64
  , active :: Bool
  , created_at :: UTCTime
  , updated_at :: Maybe UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data CreateTradableItemListing = CreateTradableItemListing
  { listing_type :: TradableItemListingType
  , batched_by :: Int16
  , unit_quantity :: Int32
  , cost :: Int64
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)
