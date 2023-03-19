module Muridae.JSON.PooledListing.Types
  ( ResListingsUnderItem (..)
  , PooledBuyListing (..)
  , PooledSellListing (..)
  )
where
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.Int (Int32, Int16, Int64)
import Data.Scientific (Scientific)
import Servant.API (HasStatus (StatusOf))

data ResListingsUnderItem = ResListingsUnderItem
  { buy :: [PooledBuyListing]
  , sell :: [PooledBuyListing]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data PooledBuyListing = PooledBuyListing
  { cost :: Int32
  , batched_by :: Int16
  , unit_quantity :: Int64
  , individual_cost :: Scientific
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

data PooledSellListing = PooledSellListing
  { cost :: Int32
  , batched_by :: Int16
  , unit_quantity :: Int64
  , individual_cost :: Scientific
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------

instance HasStatus ResListingsUnderItem where
  type StatusOf ResListingsUnderItem = 200
