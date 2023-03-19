module Muridae.JSON.PooledListing
  ( serializeBuyListing
  , serializeSellListing
  )
where

--------------------------------------------------------------------------------

import Muridae.ItemListing.Types qualified as ItemListingDomain
import Muridae.ItemListing.Types
  ( unBatchedBy
  , unCost
  , unIndividualCost
  , unUnitQuantity'
  )
import qualified Muridae.ItemListing.Types as Domain
import qualified Muridae.JSON.PooledListing.Types as JSON

--------------------------------------------------------------------------------

serializeBuyListing :: Domain.PooledBuyListing -> JSON.PooledBuyListing
serializeBuyListing pooledBuy =
  JSON.PooledBuyListing
    (unCost pooledBuy.cost)
    (unBatchedBy pooledBuy.batchedBy)
    (unUnitQuantity' pooledBuy.unitQuantity)
    (unIndividualCost pooledBuy.individualCost)

serializeSellListing :: Domain.PooledSellListing -> JSON.PooledSellListing
serializeSellListing pooledSell =
  JSON.PooledSellListing
    (unCost pooledSell.cost)
    (unBatchedBy pooledSell.batchedBy)
    (unUnitQuantity' pooledSell.unitQuantity)
    (unIndividualCost pooledSell.individualCost)
