module Muridae.JSON.Item (module Muridae.JSON.Item) where

import Data.Coerce (coerce)
import Data.Vector (Vector)
import Muridae.Item.Id qualified as Domain
import Muridae.Item.Types qualified as Domain
  ( Item
      ( Item
      , createdAt
      , deletedAt
      , description
      , id
      , name
      , updatedAt
      , wikiLink
      )
  )
import Muridae.ItemListing.Types qualified as Domain
import Muridae.JSON.Item.Types qualified as JSON
import Muridae.JSON.PooledListing qualified as JSON

-- | Serializes an @Item@
serializeItem :: Domain.Item -> JSON.Item
serializeItem (Domain.Item (Domain.ItemId itemId) name desc wiki createdAt updatedAt deletedAt) =
  JSON.Item (JSON.ItemId itemId) name desc wiki createdAt updatedAt deletedAt

serializeItemWithPools
  :: ( Domain.Item
     , Vector Domain.PooledBuyListing
     , Vector Domain.PooledSellListing
     )
  -> JSON.ItemDetails
serializeItemWithPools (item, buys, sells) =
  JSON.ItemDetails
    (JSON.ItemId (coerce item.id))
    item.name
    item.description
    item.wikiLink
    (JSON.serializeBuyListing <$> buys)
    (JSON.serializeSellListing <$> sells)
    item.createdAt
    item.updatedAt
    item.deletedAt
