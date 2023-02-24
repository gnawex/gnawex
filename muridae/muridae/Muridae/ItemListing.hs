module Muridae.ItemListing
  ( indexItemListings
  , runManageItemListingDB
  , serializePooledBuys
  , serializePooledSells
  )
where

import Data.Int (Int16, Int32, Int64)
import Data.Kind (Type)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Vector (Vector)
import Effectful (Eff, Effect, (:>))
import Effectful.Dispatch.Dynamic (interpret, send)
import Effectful.Error.Static (Error, throwError)
import Muridae.DB (DB, UsageError)
import Muridae.DB.ItemListing qualified as ItemListingDB
import Muridae.Item.Id (ItemId (ItemId))
import Muridae.ItemListing.Id (ItemListingId (ItemListingId))
import Muridae.ItemListing.Types
  ( ItemListing (ItemListing)
  , ItemListingParseError (ItemListingParseError)
  , ItemListingType (Buy, Sell)
  , ManageItemListing (IndexItemListings)
  , PooledBuyListing (PooledBuyListing)
  , PooledSellListing (PooledSellListing)
  , mkBatchedBy
  , mkCost
  , mkIndividualCost
  , mkUnitQuantity
  , mkUnitQuantity'
  )
import Muridae.User.Id (UserId (UserId))

--------------------------------------------------------------------------------

indexItemListings
  :: ManageItemListing :> es
  => Eff es (Vector ItemListing)
indexItemListings = send IndexItemListings

-- TODO: Log effect?
runManageItemListingDB
  :: forall (es :: [Effect]) (a :: Type)
   . ( DB :> es
     , Error ItemListingParseError :> es
     , Error UsageError :> es
     )
  => Eff (ManageItemListing : es) a
  -> Eff es a
runManageItemListingDB = interpret $ \_ -> \case
  IndexItemListings ->
    ItemListingDB.index >>= either throwError parseItemListings

--------------------------------------------------------------------------------

serializePooledBuys
  :: (Int32, Int16, Int64, Scientific) -> Maybe PooledBuyListing
serializePooledBuys (cost, batchedBy, summedUnitQuantity, individualCost) =
  pure PooledBuyListing
    <*> mkCost cost
    <*> mkBatchedBy batchedBy
    <*> mkUnitQuantity' summedUnitQuantity
    <*> mkIndividualCost individualCost

serializePooledSells
  :: (Int32, Int16, Int64, Scientific) -> Maybe PooledSellListing
serializePooledSells (cost, batchedBy, summedUnitQuantity, individualCost) =
  pure PooledSellListing
    <*> mkCost cost
    <*> mkBatchedBy batchedBy
    <*> mkUnitQuantity' summedUnitQuantity
    <*> mkIndividualCost individualCost

parseItemListing
  :: ( Int64
     , Int64
     , Int64
     , Text
     , Text
     , Int16
     , Int32
     , Int32
     , Int32
     , Bool
     , UTCTime
     , Maybe UTCTime
     )
  -> Maybe ItemListing
parseItemListing
  ( listingId
    , itemId
    , userId
    , username
    , listingType
    , batchedBy
    , unitQuantity
    , currentUnitQuantity
    , cost
    , active
    , createdAt
    , updatedAt
    ) =
    pure ItemListing
      <*> Just (ItemListingId listingId)
      <*> Just (ItemId itemId)
      <*> Just (UserId userId)
      <*> Just username
      <*> parseItemListingType listingType
      <*> mkBatchedBy batchedBy
      <*> mkUnitQuantity unitQuantity
      <*> mkUnitQuantity currentUnitQuantity
      <*> mkCost cost
      <*> Just active
      <*> Just createdAt
      <*> Just updatedAt

parseItemListingType :: Text -> Maybe ItemListingType
parseItemListingType = \case
  "buy" -> Just Buy
  "sell" -> Just Sell
  _ -> Nothing

parseItemListings
  :: (Error ItemListingParseError :> es)
  => Vector
      ( Int64
      , Int64
      , Int64
      , Text
      , Text
      , Int16
      , Int32
      , Int32
      , Int32
      , Bool
      , UTCTime
      , Maybe UTCTime
      )
  -> Eff es (Vector ItemListing)
parseItemListings =
  maybe
    (throwError @ItemListingParseError ItemListingParseError)
    pure
    . mapM parseItemListing
