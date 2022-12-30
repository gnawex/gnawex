module DB.TradableItemListing where

import DB (muridaeDB)
import DB.Types qualified as DB
import Data.Coerce (coerce)
import Data.Functor.Identity (Identity)
import Database.Beam.Postgres (Pg)
import Database.Beam.Query (
  all_,
  default_,
  insert,
  insertExpressions,
  runInsert,
  runSelectReturningList,
  select,
  val_,
 )
import MuridaeWeb.Handler.Item.Listing.Types qualified as Handler
import MuridaeWeb.Handler.Item.Types qualified as Handler
import MuridaeWeb.Handler.User qualified as Handler

all :: Pg [DB.TradableItemListing Identity]
all = runSelectReturningList (select (all_ (DB.muridaeTradableItemListings muridaeDB)))

create ::
  Handler.UserId ->
  Handler.TradableItemId ->
  Handler.CreateTradableItemListing ->
  Pg ()
create userId tradableItemId handlerParams = do
  runInsert . insert (DB.muridaeTradableItemListings muridaeDB) $
    insertExpressions
      [ DB.TradableItemListing
          default_
          (DB.TradableItemIdPk . val_ . coerce $ tradableItemId)
          (DB.UserIdPk . val_ . coerce $ userId)
          (val_ . parseType $ handlerParams.listing_type)
          (val_ handlerParams.batched_by)
          (val_ handlerParams.unit_quantity)
          (val_ handlerParams.cost)
          (val_ True)
          default_
          (val_ Nothing)
      ]
 where
  parseType :: Handler.TradableItemListingType -> DB.ListingType
  parseType = \case
    Handler.BUY -> DB.Buy
    Handler.SELL -> DB.Sell
