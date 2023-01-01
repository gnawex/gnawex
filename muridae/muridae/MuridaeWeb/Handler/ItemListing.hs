module MuridaeWeb.Handler.ItemListing (
  getListingsOfItem,
  create,
  updateStatus,
  index,
) where

import DB.Types qualified as DB
import Data.Coerce (coerce)
import Data.Functor.Identity
import Effectful (liftIO)
import Effectful.Beam (queryDebug)
import Effectful.Error.Static (throwError)
import Muridae.Model.TradableItemListing qualified as Model.ItemListing
import MuridaeWeb.Handler.Item.Types (TradableItemId (TradableItemId))
import MuridaeWeb.Handler.ItemListing.Types (
  CreateTradableItemListing,
  PooledListing (PooledListing),
  ReqStatus,
  ResListingsUnderItem (ResListingsUnderItem),
  TradableItemListing (
    TradableItemListing,
    active,
    batched_by,
    cost,
    created_at,
    id,
    listing_type,
    owner_id,
    tradable_item_id,
    unit_quantity,
    updated_at
  ),
  TradableItemListingId (TradableItemListingId),
  TradableItemListingType (BUY, SELL),
 )
import MuridaeWeb.Handler.User (UserId (UserId))
import MuridaeWeb.Types (Handler')
import Servant (ServerError (ServerError))
import Servant.API.ContentTypes (NoContent (NoContent))

-------------------------------------------------------------------------------
-- Item listing handlers

index :: Handler' [TradableItemListing]
index =
  queryDebug putStrLn Model.ItemListing.all
    >>= pure . (fmap parseDBItemListing)

-- | Get all the listings under a tradable item
getListingsOfItem :: TradableItemId -> Handler' ResListingsUnderItem
getListingsOfItem itemId =
  queryDebug putStrLn $ do
    (pooledBuy, pooledSell) <- Model.ItemListing.getListingsUnderItem (coerce itemId)

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
      queryDebug print (Model.ItemListing.create userId' params)
        >>= liftIO . print
        >> pure NoContent
    Nothing -> pure NoContent

updateStatus ::
  Maybe UserId ->
  TradableItemListingId ->
  ReqStatus ->
  Handler' TradableItemListing
updateStatus userId listingId params =
  case userId of
    Just userId' -> do
      dbListing <-
        queryDebug
          putStrLn
          (Model.ItemListing.updateStatus userId' listingId params)

      case dbListing of
        Just dbListing' -> pure $ parseDBItemListing dbListing'
        Nothing -> throwError @ServerError (ServerError 404 "Item listing not found" "" [])
    -- TODO: Replace (auth context)
    Nothing -> throwError @ServerError (ServerError 401 "No permission" "" [])

-------------------------------------------------------------------------------
-- Helper functions

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

fromDbListingType :: DB.ListingType -> TradableItemListingType
fromDbListingType dbListingType =
  case dbListingType of
    DB.Buy -> BUY
    DB.Sell -> SELL
