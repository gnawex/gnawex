module MuridaeWeb.JSON.PooledListing
  ( PooledBuyListing (..)
  , PooledSellListing (..)
  , ResListingsUnderItem (..)
  , fromDbPooledBuyListing
  , fromDbPooledSellListing
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int16, Int32)
import Database.Beam (Generic)
import Muridae.ItemListing.Types qualified as DB
import Servant (HasStatus (StatusOf))

data ResListingsUnderItem = ResListingsUnderItem
  { buy :: [PooledBuyListing]
  , sell :: [PooledBuyListing]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data PooledBuyListing = PooledBuyListing
  { cost :: Int32
  , batched_by :: Int16
  , unit_quantity :: Int32
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

data PooledSellListing = PooledSellListing
  { cost :: Int32
  , batched_by :: Int16
  , unit_quantity :: Int32
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------

instance HasStatus ResListingsUnderItem where
  type StatusOf ResListingsUnderItem = 200

--------------------------------------------------------------------------------

fromDbPooledBuyListing :: DB.PooledBuyListing -> PooledBuyListing
fromDbPooledBuyListing (DB.PooledBuyListing cost batchedBy unitQuantity) =
  PooledBuyListing cost batchedBy unitQuantity

fromDbPooledSellListing :: DB.PooledSellListing -> PooledSellListing
fromDbPooledSellListing (DB.PooledSellListing cost batchedBy unitQuantity) =
  PooledSellListing cost batchedBy unitQuantity
