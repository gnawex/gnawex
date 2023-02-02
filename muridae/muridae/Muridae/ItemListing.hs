{-# LANGUAGE ScopedTypeVariables #-}

module Muridae.ItemListing
  ( list
  , create
  , itemListings
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
import Muridae.Item.Types (Item, ItemId (ItemId), PrimaryKey (ItemPk))
import Muridae.ItemListing.Model qualified as ItemListing
import Muridae.ItemListing.Types
  ( FilterItemListingType
  , ItemListingId (ItemListingId)
  , ListingType (Buy, Sell)
  )
import Muridae.ItemListing.Types qualified as DB
import Muridae.User.Types (PrimaryKey (UserPk), UserId (UserId))
import MuridaeWeb.Handler.Item.Types qualified as Handler
import MuridaeWeb.Handler.ItemListing.Types
  ( CreateItemListing
  , ItemListingId (ItemListingId)
  , ItemListingType (BUY, SELL)
  , ReqStatus
  )
import MuridaeWeb.Handler.ItemListing.Types qualified as Handler
import MuridaeWeb.Handler.User (UserId (UserId))
import MuridaeWeb.Handler.User qualified as UserHandler

-------------------------------------------------------------------------------

list
  :: forall (es :: [Effect])
   . (DB :> es, Error DbError :> es)
  => FilterItemListingType
  -> Eff es [Handler.ItemListing]
list filterType =
  queryDebug putStrLn (ItemListing.listAll Nothing filterType)
    >>= \listing -> pure $ parseDBItemListing <$> listing

itemListings
  :: forall (es :: [Effect])
   . (DB :> es, Error DbError :> es)
  => ItemId
  -> FilterItemListingType
  -> Eff es [Handler.ItemListing]
itemListings itemId filterType =
  queryDebug putStrLn (ItemListing.listAll (Just itemId) filterType)
    >>= \listing -> pure $ parseDBItemListing <$> listing

create
  :: (DB :> es, Error DbError :> es)
  => UserHandler.UserId
  -> CreateItemListing
  -> Eff es [(DB.ItemListing Identity, Int32, Int32)]
create userId params = do
  authQueryDebug putStrLn (coerce userId) $ do
    listing <- ItemListing.create userId params
    matchedListings <- ItemListing.findMatches listing

    ItemListing.match listing matchedListings

updateStatus
  :: (DB :> es, Error DbError :> es)
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
    , tradable_item_id =
        coerce
          @(PrimaryKey Item Identity)
          @Handler.ItemId
          dbItemListing._tradable_item
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
