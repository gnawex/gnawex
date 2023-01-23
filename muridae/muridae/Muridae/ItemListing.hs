module Muridae.ItemListing
  ( list
  , create
  , getListingsUnderItem
  , updateStatus
  )
where

import Data.Coerce (coerce)
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity)
import Data.Int (Int32)
import Effectful (Eff, type (:>))
import Effectful.Beam (DB, authQueryDebug, queryDebug)
import Muridae.Item.Types (ItemId (ItemId), PrimaryKey (ItemPk))
import Muridae.ItemListing.Model qualified as ItemListing
import qualified Muridae.ItemListing.Types as DB
import Muridae.ItemListing.Types
  ( ItemListingId (ItemListingId)
  , ListingType (Buy, Sell)
  )
import Muridae.User.Types (PrimaryKey (UserPk), UserId (UserId))
import MuridaeWeb.Handler.Item.Types (ItemId (ItemId))
import MuridaeWeb.Handler.Item.Types qualified as Handler

import MuridaeWeb.Handler.ItemListing.Types
  ( CreateItemListing
  , PooledListing (PooledListing)
  , ReqStatus
  , ResListingsUnderItem (ResListingsUnderItem)
  , ItemListingId (ItemListingId)
  , ItemListingType (BUY, SELL)
  )
import MuridaeWeb.Handler.ItemListing.Types qualified as Handler
import MuridaeWeb.Handler.User (UserId (UserId))
import MuridaeWeb.Handler.User qualified as UserHandler

-------------------------------------------------------------------------------

list :: (DB :> es) => Eff es [Handler.ItemListing]
list = queryDebug putStrLn ItemListing.listAll <&> fmap parseDBItemListing

create
  :: (DB :> es)
  => UserHandler.UserId
  -> CreateItemListing
  -> Eff es [(DB.ItemListing Identity, Int32, Int32)]
create userId params = do
  authQueryDebug putStrLn (coerce userId) $ do
    listing <- ItemListing.create userId params
    matchedListings <- ItemListing.findMatches listing

    ItemListing.match listing matchedListings

getListingsUnderItem
  :: (DB :> es) => Handler.ItemId -> Eff es ResListingsUnderItem
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
  -> Handler.ItemListingId
  -> ReqStatus
  -> Eff es (Maybe Handler.ItemListing)
updateStatus userId listingId params =
  queryDebug
    putStrLn
    (ItemListing.updateStatus userId listingId params)
    <&> fmap parseDBItemListing

-------------------------------------------------------------------------------

parseDBItemListing :: DB.ItemListing Identity -> Handler.ItemListing
parseDBItemListing dbItemListing =
  Handler.ItemListing
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

fromDbListingType :: ListingType -> ItemListingType
fromDbListingType dbListingType =
  case dbListingType of
    Buy -> BUY
    Sell -> SELL
