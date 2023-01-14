module Muridae.ItemListing (list, create, getListingsUnderItem, updateStatus) where

import Data.Coerce (coerce)
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity)
import Data.Int (Int32)
import Effectful (Eff, type (:>))
import Effectful.Beam (DB, authQueryDebug, queryDebug)
import Muridae.Item.Types (ItemId (ItemId), PrimaryKey (ItemPk))
import Muridae.ItemListing.Model qualified as ItemListing
import Muridae.ItemListing.Types
  ( ItemListing
      ( _active
      , _batched_by
      , _cost
      , _created_at
      , _id
      , _tradable_item
      , _type
      , _unit_quantity
      , _updated_at
      , _user
      )
  , ItemListingId (ItemListingId)
  , ListingType (Buy, Sell)
  )
import Muridae.User.Types (PrimaryKey (UserPk), UserId (UserId))
import MuridaeWeb.Handler.Item.Types (TradableItemId (TradableItemId))
import MuridaeWeb.Handler.Item.Types qualified as ItemHandler
import MuridaeWeb.Handler.ItemListing.Types
  ( CreateTradableItemListing
  , PooledListing (PooledListing)
  , ReqStatus
  , ResListingsUnderItem (ResListingsUnderItem)
  , TradableItemListing
    ( TradableItemListing
    , active
    , batched_by
    , cost
    , created_at
    , id
    , listing_type
    , owner_id
    , tradable_item_id
    , unit_quantity
    , updated_at
    )
  , TradableItemListingId (TradableItemListingId)
  , TradableItemListingType (BUY, SELL)
  )
import MuridaeWeb.Handler.User (UserId (UserId))
import MuridaeWeb.Handler.User qualified as UserHandler

-------------------------------------------------------------------------------

list :: (DB :> es) => Eff es [TradableItemListing]
list = queryDebug putStrLn ItemListing.all <&> fmap parseDBItemListing

create
  :: (DB :> es)
  => UserHandler.UserId
  -> CreateTradableItemListing
  -> Eff es [(ItemListing Identity, Int32, Int32)]
create userId params = do
  authQueryDebug putStrLn (coerce userId) $ do
    listing <- ItemListing.create userId params
    matchedListings <- ItemListing.findMatches listing

    ItemListing.match listing matchedListings

getListingsUnderItem
  :: (DB :> es) => ItemHandler.TradableItemId -> Eff es ResListingsUnderItem
getListingsUnderItem itemId =
  queryDebug putStrLn $ do
    (pooledBuy, pooledSell) <- ItemListing.getListingsUnderItem (coerce itemId)

    let
      pooledBuy' = toPooledListing <$> pooledBuy
      pooledSell' = toPooledListing <$> pooledSell

    pure (ResListingsUnderItem pooledBuy' pooledSell')
 where
  toPooledListing (_, lCo, lBa, lQt) = PooledListing lCo lBa lQt

updateStatus
  :: (DB :> es)
  => UserHandler.UserId
  -> TradableItemListingId
  -> ReqStatus
  -> Eff es (Maybe TradableItemListing)
updateStatus userId listingId params =
  queryDebug
    putStrLn
    (ItemListing.updateStatus userId listingId params)
    <&> fmap parseDBItemListing

-------------------------------------------------------------------------------

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
