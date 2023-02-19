module MuridaeWeb.JSON.PooledListing
  ( PooledBuyListing (..)
  , PooledSellListing (..)
  , ResListingsUnderItem (..)
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int16, Int32)
import Database.Beam (Generic)
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
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

data PooledSellListing = PooledSellListing
  { cost :: Int32
  , batched_by :: Int16
  , unit_quantity :: Int32
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------

instance HasStatus ResListingsUnderItem where
  type StatusOf ResListingsUnderItem = 200
