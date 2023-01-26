module MuridaeWeb.Handler.ItemListing.Types (module MuridaeWeb.Handler.ItemListing.Types) where

import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Int (Int16, Int32)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import MuridaeWeb.Handler.Item.Types (ItemId)
import MuridaeWeb.Handler.User (UserId)
import Servant.API (FromHttpApiData, HasStatus (StatusOf))

newtype ItemListingId = ItemListingId Int32
  deriving (ToJSON, FromHttpApiData) via Int32

data ItemListingType = BUY | SELL
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

data ItemListing = ItemListing
  { id :: ItemListingId
  , tradable_item_id :: ItemId
  , owner_id :: UserId
  , listing_type :: ItemListingType
  , batched_by :: Int16
  , unit_quantity :: Int32
  , cost :: Int32
  , active :: Bool
  , created_at :: UTCTime
  , updated_at :: Maybe UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data CreateItemListing = CreateItemListing
  { item_id :: ItemId
  , listing_type :: ItemListingType
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

newtype ReqStatus = ReqStatus {active :: Bool}
  deriving stock (Generic)
  deriving anyclass (FromJSON)

-------------------------------------------------------------------------------
-- Instances

instance HasStatus ItemListing where
  type StatusOf ItemListing = 200

instance HasStatus ResListingsUnderItem where
  type StatusOf ResListingsUnderItem = 200
