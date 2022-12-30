module MuridaeWeb.Handler.ItemListing (getListingsOfItem, create) where

import DB.TradableItemListing qualified
import DB.Types qualified as DB
import Data.Coerce (coerce)
import Effectful (liftIO)
import Effectful.Beam (queryDebug)
import MuridaeWeb.Handler.Item.Types (TradableItemId (TradableItemId))
import MuridaeWeb.Handler.ItemListing.Types (
  CreateTradableItemListing,
  PooledListing (PooledListing),
  ResListingsUnderItem (ResListingsUnderItem),
 )
import MuridaeWeb.Handler.User (UserId)
import MuridaeWeb.Types (Handler')
import Servant.API.ContentTypes (NoContent (NoContent))

-------------------------------------------------------------------------------
-- Item listing handlers

-- | Get all the listings under a tradable item
getListingsOfItem :: TradableItemId -> Handler' ResListingsUnderItem
getListingsOfItem itemId =
  queryDebug putStrLn $ do
    (pooledBuy, pooledSell) <- DB.TradableItemListing.getListingsUnderItem (coerce itemId)

    let pooledBuy' = toPooledListing <$> pooledBuy
        pooledSell' = toPooledListing <$> pooledSell

    pure (ResListingsUnderItem pooledBuy' pooledSell')
 where
  toPooledListing (_, lCo, lBa, lQt) = PooledListing lCo lBa lQt

-- TODO: Use auth context
create ::
  Maybe UserId ->
  CreateTradableItemListing ->
  Handler' NoContent
create userId params =
  case userId of
    Just userId' ->
      queryDebug print (DB.TradableItemListing.create userId' params)
        >>= liftIO . print
        >> pure NoContent
    Nothing -> pure NoContent

-------------------------------------------------------------------------------
-- Helper functions

-- parseDBItemListing :: DB.TradableItemListing Identity -> TradableItemListing
-- parseDBItemListing dbItemListing =
--   TradableItemListing
--     { id = coerce dbItemListing._id
--     , tradable_item_id = coerce dbItemListing._tradable_item
--     , owner_id = coerce dbItemListing._user
--     , listing_type = fromDbListingType dbItemListing._type
--     , batched_by = dbItemListing._batched_by
--     , unit_quantity = dbItemListing._unit_quantity
--     , cost = dbItemListing._cost
--     , active = dbItemListing._active
--     , created_at = dbItemListing._created_at
--     , updated_at = dbItemListing._updated_at
--     }

-- fromDbListingType :: DB.ListingType -> TradableItemListingType
-- fromDbListingType dbListingType =
--   case dbListingType of
--     DB.Buy -> BUY
--     DB.Sell -> SELL
