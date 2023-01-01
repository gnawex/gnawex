module Muridae.ItemListing.Model where

import DB (muridaeDB)
import DB.Types qualified as DB
import Data.Coerce (coerce)
import Data.Functor.Identity (Identity)
import Data.Int (Int16, Int32)
import Database.Beam.Postgres (Pg, Postgres)
import Database.Beam.Query (
  SqlEq ((==?.)),
  SqlSelect,
  aggregate_,
  all_,
  asc_,
  default_,
  desc_,
  filter_',
  fromMaybe_,
  group_,
  insert,
  insertExpressions,
  orderBy_,
  runInsert,
  runSelectReturningList,
  runSelectReturningOne,
  runUpdate,
  select,
  sqlBool_,
  sum_,
  update,
  val_,
  (&&?.),
  (<-.),
  (==.),
 )
import Database.Beam.Schema (primaryKey)
import Muridae.Item.Types (ItemId (ItemId), PrimaryKey (ItemPk))
import Muridae.ItemListing.Types (
  ItemListing (ItemListing, _active, _batched_by, _cost, _tradable_item, _type, _unit_quantity),
  ItemListingId (ItemListingId),
  ListingType (Buy, Sell),
  PrimaryKey (ItemListingPk),
 )
import Muridae.User.Types (PrimaryKey (UserPk), UserId (UserId))
import MuridaeWeb.Handler.Item.Types qualified as Handler
import MuridaeWeb.Handler.ItemListing.Types qualified as Handler
import MuridaeWeb.Handler.User qualified as Handler

-------------------------------------------------------------------------------
-- Item listing DB functions

all :: Pg [ItemListing Identity]
all = runSelectReturningList (select (all_ (DB.muridaeTradableItemListings muridaeDB)))

getListingsUnderItem ::
  ItemId ->
  Pg
    ( [(ListingType, Int32, Int16, Int32)]
    , [(ListingType, Int32, Int16, Int32)]
    )
getListingsUnderItem itemId = do
  pooledBuy <- runSelectReturningList (groupListings Buy itemId)
  pooledSell <- runSelectReturningList (groupListings Sell itemId)

  pure (pooledBuy, pooledSell)

create ::
  Handler.UserId ->
  Handler.CreateTradableItemListing ->
  Pg ()
create userId handlerParams = do
  runInsert . insert (DB.muridaeTradableItemListings muridaeDB) $
    insertExpressions
      [ ItemListing
          default_
          -- TODO: Maybe get rid of this, and look for the item first via query
          ( ItemPk
              . val_
              . coerce @Handler.TradableItemId @ItemId
              $ handlerParams.item_id
          )
          (UserPk . val_ . coerce $ userId)
          (val_ . fromHandlerListingType $ handlerParams.listing_type)
          (val_ handlerParams.batched_by)
          (val_ handlerParams.unit_quantity)
          (val_ handlerParams.cost)
          (val_ True)
          default_
          (val_ Nothing)
      ]
 where

updateStatus ::
  Handler.UserId ->
  Handler.TradableItemListingId ->
  Handler.ReqStatus ->
  Pg (Maybe (ItemListing Identity))
updateStatus _userId listingId params = do
  runUpdate $
    update
      (DB.muridaeTradableItemListings muridaeDB)
      (\listing -> mconcat [listing._active <-. val_ params.active])
      (\listing -> primaryKey listing ==. (toListingPk listingId))

  updatedListing <-
    runSelectReturningOne $
      select $
        all_ (DB.muridaeTradableItemListings muridaeDB)

  pure updatedListing
 where
  toListingPk listingId' =
    ItemListingPk
      . val_
      . coerce @Handler.TradableItemListingId @ItemListingId
      $ listingId'

-------------------------------------------------------------------------------
-- Query helper functions

{- | Groups listings of a specific item. This will find listings of certain
 price points, and group the same ones together to provide how many overall
 units are there being sold at that batch & cost.
-}
groupListings ::
  ListingType ->
  ItemId ->
  SqlSelect
    Postgres
    (ListingType, Int32, Int16, Int32)
groupListings listingType itemId =
  select
    $ orderBy_
      ( \(_type, cost, batched_by, _) ->
          case listingType of
            Buy -> (asc_ batched_by, desc_ cost)
            Sell -> (asc_ batched_by, desc_ cost)
      )
    $ aggregate_
      ( \listing ->
          let totalQuantityToBeExchanged =
                -- NOTE: Would have to test if the sum of listings will
                -- overflow 32 bits. If so, maybe consider casting it to Int64
                -- (bigint), or Scientific (numeric)?
                fromMaybe_
                  (val_ 0)
                  (sum_ listing._unit_quantity)
           in ( group_ listing._type
              , group_ listing._cost
              , group_ listing._batched_by
              , totalQuantityToBeExchanged
              )
      )
    $ filter_'
      ( \listing ->
          (listing._type ==?. (val_ listingType))
            &&?. (listing._tradable_item ==?. (ItemPk . val_ . coerce $ itemId))
            &&?. (sqlBool_ listing._active)
      )
    $ all_ (DB.muridaeTradableItemListings muridaeDB)

-------------------------------------------------------------------------------
-- Non-query helper functions

fromHandlerListingType :: Handler.TradableItemListingType -> ListingType
fromHandlerListingType = \case
  Handler.BUY -> Buy
  Handler.SELL -> Sell
