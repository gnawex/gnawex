module Muridae.ItemTxn.Model where

import DB (muridaeDB)
import DB.Types (muridaeTradableItemTransactions)
import Data.Bool (bool)
import Data.Functor.Identity (Identity)
import Data.Int (Int32)
import Data.Kind (Type)
import Database.Beam (
  QExpr,
  Table (primaryKey),
  default_,
  insert,
  insertExpressions,
  val_,
 )
import Database.Beam.Backend.SQL.BeamExtensions (runInsertReturningList)
import Database.Beam.Postgres (Pg, Postgres)
import Muridae.ItemListing.Types (
  ItemListing (
    _current_unit_quantity,
    _type,
    _user
  ),
  ListingType (Buy, Sell),
 )
import Muridae.ItemTxn.Types (ItemTxn (ItemTxn), Status (Pending))

create ::
  ItemListing Identity ->
  [(ItemListing Identity, Int32, Int32)] ->
  Pg [ItemTxn Identity]
create createdListing matchedListings =
  runInsertReturningList $
    insert
      muridaeDB.muridaeTradableItemTransactions
      (insertExpressions (fromItemListing createdListing <$> matchedListings))

fromItemListing ::
  forall (s :: Type).
  ItemListing Identity ->
  (ItemListing Identity, Int32, Int32) ->
  ItemTxn (QExpr Postgres s)
fromItemListing createdListing (matchedListing, matchedRunningAmount, matchedTotalQty) =
  ItemTxn
    default_
    (primaryKey $ val_ buyListing)
    (primaryKey $ val_ sellListing)
    (val_ transactedQty)
    (val_ Pending)
    (val_ buyListing._user)
    (val_ sellListing._user)
    default_
    (val_ Nothing)
 where
  transactedQty =
    matchedListing._current_unit_quantity
      - computeNewMatchedQty createdListing matchedRunningAmount matchedTotalQty

  buyListing = bool matchedListing createdListing (createdListing._type == Buy)
  sellListing = bool matchedListing createdListing (createdListing._type == Sell)

computeNewMatchedQty ::
  -- The listing created that GNAWEX needs to look for matches for
  ItemListing Identity ->
  -- The matched listing's running amount
  Int32 ->
  -- Total quantity of matches that GNAWEX was able to find
  Int32 ->
  -- The new quantity of the matched listing
  Int32
computeNewMatchedQty createdListing runningAmount totalMatchedQuantity =
  bool
    (max (runningAmount - totalMatchedQuantity) 0)
    (max (totalMatchedQuantity - createdListing._current_unit_quantity) 0)
    (runningAmount - totalMatchedQuantity == 0)
