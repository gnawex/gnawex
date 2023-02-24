module Muridae.JSON.ItemListing
  ( serializeItemListingType
  , serializeItemListing
  )
where

--------------------------------------------------------------------------------

import Muridae.Item.Id qualified as Domain
import Muridae.ItemListing.Id qualified as Domain
import Muridae.ItemListing.Types qualified as Domain
import Muridae.JSON.Item.Id qualified as JSON
import Muridae.JSON.ItemListing.Types qualified as JSON
import Muridae.JSON.User qualified as JSON
import Muridae.User.Id qualified as Domain
import Data.Coerce (coerce)

--------------------------------------------------------------------------------

serializeItemListingType :: Domain.ItemListingType -> JSON.ItemListingType
serializeItemListingType = \case
  Domain.Buy -> JSON.BUY
  Domain.Sell -> JSON.SELL

serializeItemListing :: Domain.ItemListing -> JSON.ItemListing
serializeItemListing listing =
  JSON.ItemListing
    { id = coerce @Domain.ItemListingId @JSON.ItemListingId listing.id
    , tradable_item_id = coerce @Domain.ItemId @JSON.ItemId listing.tradableItemId
    , owner_id = coerce @Domain.UserId @JSON.UserId listing.userId
    , owner = JSON.User (coerce listing.userId) listing.username
    , listing_type = serializeItemListingType listing.listingType
    , batched_by = Domain.unBatchedBy listing.batchedBy
    , unit_quantity = Domain.unUnitQuantity listing.unitQuantity
    , cost = Domain.unCost listing.cost
    , active = listing.active
    , created_at = listing.createdAt
    , updated_at = listing.updatedAt
    }
