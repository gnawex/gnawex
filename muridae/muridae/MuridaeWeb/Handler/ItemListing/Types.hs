module MuridaeWeb.Handler.ItemListing.Types where

import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Int (Int16, Int32)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import MuridaeWeb.Handler.Item.Types (TradableItemId)
import MuridaeWeb.Handler.User (UserId)
import Servant.API (FromHttpApiData)

newtype TradableItemListingId = TradableItemListingId Int32
  deriving (ToJSON, FromHttpApiData) via Int32

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
  , cost :: Int32
  , active :: Bool
  , created_at :: UTCTime
  , updated_at :: Maybe UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data CreateTradableItemListing = CreateTradableItemListing
  { item_id :: TradableItemId
  , listing_type :: TradableItemListingType
  , batched_by :: Int16
  , unit_quantity :: Int32
  , cost :: Int32
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

data PooledListing = PooledListing
  { cost :: Int32
  , batched_by :: Int16
  , unit_quantity :: Int32
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data ResListingsUnderItem = ResListingsUnderItem
  { buy :: [PooledListing]
  , sell :: [PooledListing]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data ReqStatus = ReqStatus {active :: Bool}
  deriving stock (Generic)
  deriving anyclass (FromJSON)
