{-# LANGUAGE ScopedTypeVariables #-}

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
import Effectful (Eff, Effect, type (:>))
import Effectful.Beam (DB, DbError, authQueryDebug, queryDebug)
import Effectful.Error.Static (Error)
import Muridae.Item.Types (ItemId (ItemId), PrimaryKey (ItemPk))
import Muridae.ItemListing.Model qualified as ItemListing
import Muridae.ItemListing.Types
  ( ItemListingId (ItemListingId)
  , ListingType (Buy, Sell)
  )
import Muridae.ItemListing.Types qualified as DB
import Muridae.User.Types (PrimaryKey (UserPk), UserId (UserId))
import MuridaeWeb.Handler.Item.Types (ItemId (ItemId))
import MuridaeWeb.Handler.Item.Types qualified as Handler
import MuridaeWeb.Handler.ItemListing.Types
  ( CreateItemListing
  , ItemListingId (ItemListingId)
  , ItemListingType (BUY, SELL)
  , PooledListing (PooledListing)
  , ReqStatus
  , ResListingsUnderItem (ResListingsUnderItem)
  )
import MuridaeWeb.Handler.ItemListing.Types qualified as Handler
import MuridaeWeb.Handler.User (UserId (UserId))
import MuridaeWeb.Handler.User qualified as UserHandler

-------------------------------------------------------------------------------

-- data DbException = DbException

list
  :: forall (es :: [Effect])
   . (DB :> es)
  => Eff (Error DbError : es) [Handler.ItemListing]
list =
  queryDebug putStrLn ItemListing.listAll
    >>= \listing -> pure $ parseDBItemListing <$> listing

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
  :: forall (es :: [Effect])
   . (DB :> es)
  => Handler.ItemId
  -> Eff (Error DbError : es) ResListingsUnderItem
getListingsUnderItem itemId = do
  (pooledBuy, pooledSell) <-
    queryDebug putStrLn (ItemListing.getListingsUnderItem (coerce itemId))

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
  -> Eff (Error DbError : es) (Maybe Handler.ItemListing)
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
