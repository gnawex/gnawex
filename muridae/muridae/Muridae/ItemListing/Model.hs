{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Muridae.ItemListing.Model
  ( match
  , updateStatus
  , updateCurrentQuantity
  , create
  , getPooledListingsUnderItem
  , listAll
  , findMatches
  , greatest
  )
where

import Control.Monad ((<=<))
import DB (muridaeDB)
import DB.Types qualified as DB
import Data.Bool (bool)
import Data.Coerce (coerce)
import Data.Functor.Identity (Identity)
import Data.Int (Int16, Int32)
import Data.List (foldl')
import Database.Beam.Backend (BeamSqlBackend)
import Database.Beam.Backend.SQL.BeamExtensions
  ( runInsertReturningList
  , runUpdateReturningList
  )
import Database.Beam.Postgres (Pg, Postgres)
import Database.Beam.Query
  ( HaskellLiteralForQExpr
  , QBaseScope
  , QExpr
  , QGenExpr
  , QValueContext
  , SqlEq ((/=.), (==?.))
  , SqlOrd ((<=.), (>.))
  , SqlSelect
  , SqlValable
  , aggregate_
  , all_
  , asc_
  , bool_
  , default_
  , desc_
  , filter_
  , filter_'
  , frame_
  , fromMaybe_
  , group_
  , in_
  , insert
  , insertExpressions
  , noBounds_
  , noOrder_
  , noPartition_
  , orderBy_
  , over_
  , partitionBy_
  , reuse
  , runSelectReturningList
  , runSelectReturningOne
  , runUpdate
  , select
  , selectWith
  , selecting
  , sqlBool_
  , sum_
  , update
  , val_
  , withWindow_
  , (&&.)
  , (&&?.)
  , (<-.)
  , (==.)
  , (||.)
  )
import Database.Beam.Schema (Columnar, primaryKey)
import Muridae.Item.Types (ItemId (ItemId), PrimaryKey (ItemPk))
import Muridae.ItemListing.Types
  ( FilterItemListingType (ByBoth, ByBuy, BySell)
  , ItemListing
    ( ItemListing
    , _active
    , _batched_by
    , _cost
    , _current_unit_quantity
    , _id
    , _tradable_item
    , _type
    , _unit_quantity
    , _user
    )
  , ItemListingId (ItemListingId)
  , ListingType (Buy, Sell)
  , PrimaryKey (ItemListingPk)
  )
import Muridae.ItemTxn.Model qualified as ItemTxnModel
import Muridae.User.Types (PrimaryKey (UserPk), UserId (UserId))
import MuridaeWeb.Handler.Item.Types qualified as Handler
import MuridaeWeb.Handler.ItemListing.Types qualified as Handler
import MuridaeWeb.Handler.User qualified as Handler

-------------------------------------------------------------------------------
-- Item listing DB functions

-- TODO: I don't like the @Maybe ItemId@ part
-- TODO: Filter by @active@
listAll :: Maybe ItemId -> FilterItemListingType -> Pg [ItemListing Identity]
listAll itemId filterType =
  runSelectReturningList
    $ select
    $ filter_
      (\row -> byListingType filterType row &&. byItemId itemId row)
    $ all_
      (muridaeDB.muridaeTradableItemListings)
 where
  byListingType filterType' row =
    case filterType' of
      ByBuy -> row._type ==. val_ Buy
      BySell -> row._type ==. val_ Sell
      ByBoth -> row._type ==. val_ Buy ||. row._type ==. val_ Sell

getPooledListingsUnderItem
  :: ItemId
  -> Pg
      ( [(ListingType, Int32, Int16, Int32)]
      , [(ListingType, Int32, Int16, Int32)]
      )
getPooledListingsUnderItem itemId = do
  pooledBuy <- runSelectReturningList (groupListings Buy itemId)
  pooledSell <- runSelectReturningList (groupListings Sell itemId)

  pure (pooledBuy, pooledSell)

create
  :: Handler.UserId
  -> Handler.CreateItemListing
  -> Pg (ItemListing Identity)
create userId handlerParams = do
  -- FIXME: I mean ideally this ([a] -> a) is ok but maybe this should be
  -- handled properly?
  ( (pure . head)
      <=< ( runInsertReturningList
              . insert (muridaeDB.muridaeTradableItemListings)
          )
    )
    $ insertExpressions
      [ ItemListing
          default_
          -- TODO: Maybe get rid of this, and look for the item first via query
          ( ItemPk
              . val_
              . coerce @Handler.ItemId @ItemId
              $ handlerParams.item_id
          )
          (UserPk . val_ . coerce $ userId)
          (val_ . fromHandlerListingType $ handlerParams.listing_type)
          (val_ handlerParams.batched_by)
          (val_ handlerParams.unit_quantity)
          (val_ handlerParams.unit_quantity)
          (val_ handlerParams.cost)
          (val_ True)
          default_
          (val_ Nothing)
      ]

updateStatus
  :: Handler.UserId
  -> Handler.ItemListingId
  -> Handler.ReqStatus
  -> Pg (Maybe (ItemListing Identity))
updateStatus _userId listingId params = do
  runUpdate $
    update
      (muridaeDB.muridaeTradableItemListings)
      (\listing -> mconcat [listing._active <-. val_ params.active])
      (\listing -> primaryKey listing ==. toListingPk listingId)

  runSelectReturningOne $
    select $
      filter_ (\listing -> listing._id ==. (val_ . coerce $ listingId)) $
        all_ (muridaeDB.muridaeTradableItemListings)

-- | Updates a single item listing's current unit quantity
updateCurrentQuantity :: Int32 -> ItemListing Identity -> Pg ()
updateCurrentQuantity newQty oldListing =
  -- TODO: Might not need the old listing even. Just the ID?
  runUpdate $
    update
      muridaeDB.muridaeTradableItemListings
      (\dbL -> mconcat [dbL._current_unit_quantity <-. val_ newQty])
      (\dbL -> dbL._id ==. val_ oldListing._id)

-- | Attempts to look for match(es) for the given listing.
--
-- If it does find any, it creates the necessary transactions, and updates the
-- involved listings' current unit quantity.
match
  :: ItemListing Identity
  -> [(ItemListing Identity, Int32, Int32)]
  -> Pg [(ItemListing Identity, Int32, Int32)]
match listing matches = do
  _txns <- ItemTxnModel.create listing matches

  let
    (zeroQtyMatchIds, nonZeroQtyMatch) = splitMatches matches

  updateCurrentQuantity
    (newCurrentQuantity listing._current_unit_quantity matches)
    listing

  _updatedZeroMatches <-
    runUpdateReturningList $
      update
        muridaeDB.muridaeTradableItemListings
        (\l -> mconcat [l._current_unit_quantity <-. 0])
        (\l -> l._id `in_` (val_ <$> zeroQtyMatchIds))

  _ <-
    maybe
      (pure [])
      ( \(nonZero, newQty) ->
          runUpdateReturningList $
            update
              muridaeDB.muridaeTradableItemListings
              (\l -> mconcat [l._current_unit_quantity <-. val_ newQty])
              (\l -> l._id ==. val_ nonZero._id)
      )
      nonZeroQtyMatch

  pure matches
 where
  newCurrentQuantity :: Int32 -> [(ItemListing Identity, Int32, Int32)] -> Int32
  newCurrentQuantity oldCurrentQty = \case
    [] -> oldCurrentQty
    (_match, _runningAmount, totalMatchedQty) : _ ->
      max (oldCurrentQty - totalMatchedQty) 0

  -- Splits the zero and non-zero current unit quantity matches
  splitMatches
    :: [(ItemListing Identity, Int32, Int32)]
    -> ([ItemListingId], Maybe (ItemListing Identity, Int32))
  splitMatches =
    foldl'
      ( \(zeroMatches, nonZeroMatch) (matchedListing, runningAmount, totalQty) ->
          bool
            ( zeroMatches
            , Just
                ( matchedListing
                , ItemTxnModel.computeNewMatchedQty
                    listing
                    runningAmount
                    totalQty
                )
            )
            ((matchedListing._id) : zeroMatches, nonZeroMatch)
            (ItemTxnModel.computeNewMatchedQty listing runningAmount totalQty == 0)
      )
      ([], Nothing)

findMatches
  :: ItemListing Identity
  -- A triple with the item listing, new current quantity of said listing, and
  -- the total quantity of all matched listings.
  -> Pg [(ItemListing Identity, Int32, Int32)]
findMatches listing =
  runSelectReturningList (selectWith matches)
 where
  matches = do
    cteQ <- matchesCte

    pure
      $ withWindow_
        (\(l, _) -> frame_ (partitionBy_ l._tradable_item) (noOrder_ @Int32) noBounds_)
        ( \(l, runningAmount) frame ->
            let
              totalCurrentQuantity =
                fromMaybe_ 0 (sum_ l._current_unit_quantity `over_` frame)
             in
              -- TODO: Consider using a type for this
              (l, runningAmount, totalCurrentQuantity)
        )
      $ filter_
        ( \(l, runningAmount) ->
            runningAmount - l._current_unit_quantity <=. val_ listing._current_unit_quantity
        )
        (reuse cteQ)

  -- Narrow down the search by picking out the listings that fulfill the qty
  matchesCte = do
    listingsWithRunningAmount <- runningAmountCte

    selecting $
      filter_
        (narrowListings listing)
        (reuse listingsWithRunningAmount)

  -- Find all the listings that match except the quantity.
  runningAmountCte =
    selecting $
      withWindow_ orderByListingIdFrame sumCurrentQuantityOverFrame $
        filter_ (relevantListings listing) $
          all_ (muridaeDB.muridaeTradableItemListings)

-------------------------------------------------------------------------------
-- Query helper functions

-- TODO: Move this out to query module and enable missing signature warning

---- Windows/Frames

orderByListingIdFrame l =
  frame_ (noPartition_ @Int32) (Just (asc_ l._id)) noBounds_

sumCurrentQuantityOverFrame listing window =
  let
    runningAmount =
      fromMaybe_ 0 (sum_ listing._current_unit_quantity `over_` window)
   in
    (listing, runningAmount)

---- Filters

narrowListings l (dbL, runningAmount) =
  (runningAmount - coerce dbL._current_unit_quantity)
    <=. val_ l._current_unit_quantity

relevantListings listing potentialListing =
  potentialListing._active
    &&. (potentialListing._tradable_item ==. val_ listing._tradable_item)
    &&. (potentialListing._batched_by ==. val_ listing._batched_by)
    &&. (potentialListing._current_unit_quantity >. val_ 0)
    &&. (potentialListing._cost ==. val_ listing._cost)
    &&. (potentialListing._user /=. val_ listing._user)
    &&. typeFilter potentialListing listing
 where
  typeFilter dbL l =
    bool (dbL._type ==. val_ Sell) (dbL._type ==. val_ Buy) (l._type == Sell)

-- | Groups listings of a specific item. This will find listings of certain
--  price points, and group the same ones together to provide how many overall
--  units are there being sold at that batch & cost.
groupListings
  :: ListingType
  -> ItemId
  -> SqlSelect
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
          let
            totalQuantityToBeExchanged =
              -- NOTE: Would have to test if the sum of listings will
              -- overflow 32 bits. If so, maybe consider casting it to Int64
              -- (bigint), or Scientific (numeric)?
              fromMaybe_
                (val_ 0)
                (sum_ listing._unit_quantity)
           in
            ( group_ listing._type
            , group_ listing._cost
            , group_ listing._batched_by
            , totalQuantityToBeExchanged
            )
      )
    $ filter_'
      ( \listing ->
          (listing._type ==?. val_ listingType)
            &&?. (listing._tradable_item ==?. (ItemPk . val_ . coerce $ itemId))
            &&?. sqlBool_ listing._active
      )
    $ all_ (muridaeDB.muridaeTradableItemListings)

byItemId
  :: Maybe ItemId
  -> ItemListing (QExpr Postgres QBaseScope)
  -> QGenExpr QValueContext Postgres QBaseScope Bool
byItemId itemId row =
  case itemId of
    Just itemId' -> row._tradable_item ==. ItemPk (val_ itemId')
    Nothing -> val_ True

-------------------------------------------------------------------------------
-- Non-query helper functions

toListingPk
  :: ( HaskellLiteralForQExpr (Columnar f ItemListingId) ~ ItemListingId
     , SqlValable (Columnar f ItemListingId)
     )
  => Handler.ItemListingId
  -> PrimaryKey ItemListing f
toListingPk =
  ItemListingPk . val_ . coerce

fromHandlerListingType :: Handler.ItemListingType -> ListingType
fromHandlerListingType = \case
  Handler.BUY -> Buy
  Handler.SELL -> Sell

greatest
  :: BeamSqlBackend be
  => QGenExpr context be s a
  -> QGenExpr context be s a
  -> QGenExpr context be s a
greatest a b = bool_ a b (a <=. b)
