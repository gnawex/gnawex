module MuridaeWeb.Handler.ItemListing (
  getListingsOfItem,
  create,
  updateStatus,
  index,
) where

import Data.Coerce (coerce)
import Data.Functor.Identity
import Effectful (liftIO)
import Effectful.Beam (queryDebug)
import Effectful.Error.Static (throwError)
import Muridae.User.Types (UserId (UserId), PrimaryKey (UserPk))
import Muridae.Item.Types (ItemId (ItemId), PrimaryKey (ItemPk))
import Muridae.ItemListing.Model qualified as ItemListingModel
import Muridae.ItemListing.Types (ItemListing, ListingType (Buy, Sell))
import Muridae.ItemListing.Types qualified as ItemListingModel
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
import MuridaeWeb.Handler.User qualified as UserHandler (UserId (UserId))
import MuridaeWeb.Types (Handler')
import Servant (ServerError (ServerError))
import Servant.API.ContentTypes (NoContent (NoContent))

-------------------------------------------------------------------------------
-- Item listing handlers

index :: Handler' [TradableItemListing]
index =
  queryDebug putStrLn ItemListingModel.all
    >>= pure . (fmap parseDBItemListing)

-- | Get all the listings under a tradable item
getListingsOfItem :: TradableItemId -> Handler' ResListingsUnderItem
getListingsOfItem itemId =
  queryDebug putStrLn $ do
    (pooledBuy, pooledSell) <- ItemListingModel.getListingsUnderItem (coerce itemId)

    let pooledBuy' = toPooledListing <$> pooledBuy
        pooledSell' = toPooledListing <$> pooledSell

    pure (ResListingsUnderItem pooledBuy' pooledSell')
 where
  toPooledListing (_, lCo, lBa, lQt) = PooledListing lCo lBa lQt

-- TODO: Use auth context
create ::
  Maybe UserHandler.UserId ->
  CreateTradableItemListing ->
  Handler' NoContent
create userId params =
  case userId of
    Just userId' ->
      queryDebug print (ItemListingModel.create userId' params)
        >>= liftIO . print
        >> pure NoContent
    Nothing -> pure NoContent

updateStatus ::
  Maybe UserHandler.UserId ->
  TradableItemListingId ->
  ReqStatus ->
  Handler' TradableItemListing
updateStatus userId listingId params =
  case userId of
    Just userId' -> do
      dbListing <-
        queryDebug
          putStrLn
          (ItemListingModel.updateStatus userId' listingId params)

      case dbListing of
        Just dbListing' -> pure $ parseDBItemListing dbListing'
        Nothing -> throwError @ServerError (ServerError 404 "Item listing not found" "" [])
    -- TODO: Replace (auth context)
    Nothing -> throwError @ServerError (ServerError 401 "No permission" "" [])

-------------------------------------------------------------------------------
-- Helper functions

parseDBItemListing :: ItemListing Identity -> TradableItemListing
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

fromDbListingType :: ListingType -> TradableItemListingType
fromDbListingType dbListingType =
  case dbListingType of
    Buy -> BUY
    Sell -> SELL
