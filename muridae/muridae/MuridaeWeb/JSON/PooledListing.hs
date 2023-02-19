module MuridaeWeb.JSON.PooledListing
  ( PooledBuyListing (..)
  , PooledSellListing (..)
  , ResListingsUnderItem (..)
  , serializeBuyListing
  , serializeSellListing
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int16, Int32, Int64)
import Database.Beam (Generic)
import Muridae.ItemListing.Types (unBatchedBy, unCost, unUnitQuantity', unIndividualCost)
import Muridae.ItemListing.Types qualified as Domain
import Servant (HasStatus (StatusOf))
import Data.Scientific (Scientific)

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

--------------------------------------------------------------------------------

serializeBuyListing :: Domain.PooledBuyListing -> PooledBuyListing
serializeBuyListing pooledBuy =
  PooledBuyListing
    (unCost pooledBuy.cost)
    (unBatchedBy pooledBuy.batchedBy)
    (unUnitQuantity' pooledBuy.unitQuantity)
    (unIndividualCost pooledBuy.individualCost)

serializeSellListing :: Domain.PooledSellListing -> PooledSellListing
serializeSellListing pooledSell =
  PooledSellListing
    (unCost pooledSell.cost)
    (unBatchedBy pooledSell.batchedBy)
    (unUnitQuantity' pooledSell.unitQuantity)
    (unIndividualCost pooledSell.individualCost)
