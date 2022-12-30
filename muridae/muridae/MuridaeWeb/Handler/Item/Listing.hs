module MuridaeWeb.Handler.Item.Listing where

import DB.TradableItemListing qualified
import DB.Types qualified as DB
import Data.Coerce (coerce)
import Database.Beam (Identity)
import Effectful (liftIO)
import Effectful.Beam (queryDebug)
import MuridaeWeb.Handler.Item.Listing.Types (
  CreateTradableItemListing,
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
    updated_at,
    owner_id
  ),
  TradableItemListingId (TradableItemListingId),
  TradableItemListingType (BUY, SELL),
 )
import MuridaeWeb.Handler.Item.Types (TradableItemId (TradableItemId))
import MuridaeWeb.Handler.User (UserId (UserId))
import MuridaeWeb.Types (Handler')
import Servant.API.ContentTypes (NoContent (NoContent))

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
      , owner_id = coerce dbItemListing._user
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

-- TODO: Use auth context
create :: Maybe UserId -> TradableItemId -> CreateTradableItemListing -> Handler' NoContent
create userId tradableItemId params =
  case userId of
    Just userId' ->
      queryDebug print (DB.TradableItemListing.create userId' tradableItemId params)
        >>= liftIO . print
        >> pure NoContent
    Nothing -> pure NoContent
