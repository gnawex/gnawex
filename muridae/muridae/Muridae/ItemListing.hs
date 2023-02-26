module Muridae.ItemListing
  ( indexItemListings
  , createItemListing
  , updateItemListing
  , runManageItemListingDB
  , parsePooledBuys
  , parsePooledSells
  , serializeItemListingType
  )
where

import Data.Coerce (coerce)
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
  ( BatchedBy
  , Cost
  , ItemListing (ItemListing)
  , ItemListingParseError (ItemListingParseError)
  , ItemListingType (Buy, Sell)
  , ManageItemListing (CreateItemListing, IndexItemListings, UpdateItemListing)
  , PooledBuyListing (PooledBuyListing)
  , PooledSellListing (PooledSellListing)
  , UnitQuantity
  , mkBatchedBy
  , mkCost
  , mkIndividualCost
  , mkUnitQuantity
  , mkUnitQuantity'
  , unBatchedBy
  , unCost
  , unUnitQuantity
  )
import Muridae.User.Id (UserId (UserId))

--------------------------------------------------------------------------------

indexItemListings
  :: forall (es :: [Effect])
   . ManageItemListing :> es
  => Eff es (Vector ItemListing)
indexItemListings = send IndexItemListings

createItemListing
  :: forall (es :: [Effect])
   . ManageItemListing :> es
  => UserId
  -> ItemId
  -> ItemListingType
  -> BatchedBy
  -> UnitQuantity
  -> Cost
  -> Eff es ItemListing
createItemListing userId itemId listingType batchedBy unitQuantity =
  send . CreateItemListing userId itemId listingType batchedBy unitQuantity

updateItemListing
  :: (ManageItemListing :> es)
  => UserId
  -> ItemListingId
  -> Maybe UnitQuantity
  -> Maybe Bool
  -> Eff es (Maybe ItemListing)
updateItemListing userId itemId unitQuantity =
  send . UpdateItemListing userId itemId unitQuantity

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
  CreateItemListing userId itemId listingType batchedBy unitQuantity cost ->
    ItemListingDB.create
      (coerce userId)
      (coerce itemId)
      (serializeItemListingType listingType)
      (unBatchedBy batchedBy)
      (unUnitQuantity unitQuantity)
      (unCost cost)
      >>= either
        throwError
        (maybe (throwError ItemListingParseError) pure . parseItemListing)
  UpdateItemListing userId listingId unitQuantity active ->
    -- TODO: Handle case where it update nothing
    ItemListingDB.update
      (coerce userId)
      (coerce listingId)
      (unUnitQuantity <$> unitQuantity)
      active
      >>= either
        throwError
        -- TODO: Just a temporary thing
        ( \case
            Just itemListing ->
              maybe
                (throwError ItemListingParseError)
                (pure . Just)
                (parseItemListing itemListing)
            Nothing -> pure Nothing
        )

--------------------------------------------------------------------------------

parsePooledBuys
  :: (Int32, Int16, Int64, Scientific) -> Maybe PooledBuyListing
parsePooledBuys (cost, batchedBy, summedUnitQuantity, individualCost) =
  PooledBuyListing
    <$> mkCost cost
    <*> mkBatchedBy batchedBy
    <*> mkUnitQuantity' summedUnitQuantity
    <*> mkIndividualCost individualCost

parsePooledSells
  :: (Int32, Int16, Int64, Scientific) -> Maybe PooledSellListing
parsePooledSells (cost, batchedBy, summedUnitQuantity, individualCost) =
  PooledSellListing
    <$> mkCost cost
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
    ItemListing
      <$> Just (ItemListingId listingId)
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
  :: forall (es :: [Effect])
   . (Error ItemListingParseError :> es)
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

-- | Serializes @ItemListingType@ into @Text@
serializeItemListingType :: ItemListingType -> Text
serializeItemListingType = \case
  Buy -> "buy"
  Sell -> "sell"
