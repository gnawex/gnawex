{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module MuridaeWeb.Handler.Item.Listing where

import qualified DB.Types as DB
import Data.Coerce (coerce)
import Database.Beam (Identity)
import Effectful (liftIO)
import Effectful.Beam (queryDebug)
import MuridaeWeb.Handler.Item.Listing.Types (
  TradableItemListing (
    TradableItemListing,
    active,
    batched_by,
    cost,
    created_at,
    id,
    listing_type,
    tradable_item_id,
    unit_quantity,
    updated_at
  ),
  TradableItemListingId (TradableItemListingId),
  TradableItemListingType (BUY, SELL),
 )
import MuridaeWeb.Handler.Item.Types (TradableItemId (TradableItemId))
import MuridaeWeb.Types (Handler')
import qualified DB.TradableItemListing

-- | Get all the listings under a tradable item
index :: TradableItemId -> Handler' [TradableItemListing]
index _ =
  queryDebug print DB.TradableItemListing.all
    >>= \listings -> do
      liftIO $ print listings

      pure . (fmap parseDBItemListing) $ listings
 where
  parseDBItemListing :: DB.TradableItemListing Identity -> TradableItemListing
  parseDBItemListing dbItemListing =
    TradableItemListing
      { id = coerce dbItemListing._id
      , tradable_item_id = coerce dbItemListing._tradable_item
      , listing_type = fromDbListingType dbItemListing._type
      , batched_by = dbItemListing._batched_by
      , unit_quantity = dbItemListing._unit_quantity
      , cost = dbItemListing._cost
      , active = dbItemListing._active
      , created_at = dbItemListing._created_at
      , updated_at = dbItemListing._updated_at
      }

  fromDbListingType dbListingType =
    case dbListingType of
      DB.Buy -> BUY
      DB.Sell -> SELL
