module Muridae.ItemListing.Types
  ( PooledBuyListing (..)
  , PooledSellListing (..)
  , Cost
  , BatchedBy
  , UnitQuantity
  , ItemListing (..)
  , ItemListingType (..)
  , ItemListingParseError (..)
  , ManageItemListing (..)
  , mkCost
  , mkBatchedBy
  , mkUnitQuantity
  , mkUnitQuantity'
  , mkIndividualCost
  , unCost
  , unBatchedBy
  , unUnitQuantity
  , unUnitQuantity'
  , unIndividualCost
  )
where

--------------------------------------------------------------------------------

import Data.Coerce (coerce)
import Data.Int (Int16, Int32, Int64)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Vector (Vector)
import Effectful (Dispatch (Dynamic), DispatchOf, Effect)
import Muridae.Item.Id (ItemId)
import Muridae.ItemListing.Id (ItemListingId)
import Muridae.User.Id (UserId)

--------------------------------------------------------------------------------

newtype Cost = Cost Int32
  deriving stock (Eq, Show)

newtype BatchedBy = BatchedBy Int16
  deriving stock (Eq, Show)

newtype UnitQuantity = UnitQuantity Int32
  deriving stock (Eq, Show)

newtype SummedUnitQuantity = SummedUnitQuantity Int64
  deriving stock (Eq, Show)

newtype IndividualCost = IndividualCost Scientific
  deriving stock (Eq, Show)

data PooledBuyListing = PooledBuyListing
  { cost :: Cost
  , batchedBy :: BatchedBy
  , unitQuantity :: SummedUnitQuantity
  , individualCost :: IndividualCost
  }
  deriving stock (Eq, Show)

data PooledSellListing = PooledSellListing
  { cost :: Cost
  , batchedBy :: BatchedBy
  , unitQuantity :: SummedUnitQuantity
  , individualCost :: IndividualCost
  }
  deriving stock (Eq, Show)

data ItemListingType = Buy | Sell
  deriving stock (Eq, Show)

data ItemListing = ItemListing
  { id :: ItemListingId
  , tradableItemId :: ItemId
  , userId :: UserId
  , username :: Text
  , listingType :: ItemListingType
  , batchedBy :: BatchedBy
  , unitQuantity :: UnitQuantity
  , currentUnitQUantity :: UnitQuantity
  , cost :: Cost
  , active :: Bool
  , createdAt :: UTCTime
  , updatedAt :: Maybe UTCTime
  }
  deriving stock (Eq, Show)

data ManageItemListing :: Effect where
  IndexItemListings
    :: ManageItemListing m (Vector ItemListing)
  CreateItemListing
    :: UserId
    -> ItemId
    -> ItemListingType
    -> BatchedBy
    -> UnitQuantity
    -> Cost
    -> ManageItemListing m ItemListing
  UpdateItemListing
    :: UserId
    -> ItemListingId
    -> Maybe UnitQuantity
    -> Maybe Bool
    -> ManageItemListing m (Maybe ItemListing)

type instance DispatchOf ManageItemListing = 'Dynamic

--------------------------------------------------------------------------------
-- Errors

data ItemListingParseError = ItemListingParseError
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------

mkCost :: Int32 -> Maybe Cost
mkCost num
  | num > 0 = Just (Cost num)
  | otherwise = Nothing

unCost :: Cost -> Int32
unCost = coerce

mkBatchedBy :: Int16 -> Maybe BatchedBy
mkBatchedBy num
  | num > 0 = Just (BatchedBy num)
  | otherwise = Nothing

unBatchedBy :: BatchedBy -> Int16
unBatchedBy = coerce

mkUnitQuantity :: Int32 -> Maybe UnitQuantity
mkUnitQuantity num
  | num > 0 = Just (UnitQuantity num)
  | otherwise = Nothing

unUnitQuantity :: UnitQuantity -> Int32
unUnitQuantity = coerce

mkUnitQuantity' :: Int64 -> Maybe SummedUnitQuantity
mkUnitQuantity' num
  | num > 0 = Just (SummedUnitQuantity num)
  | otherwise = Nothing

unUnitQuantity' :: SummedUnitQuantity -> Int64
unUnitQuantity' = coerce

mkIndividualCost :: Scientific -> Maybe IndividualCost
mkIndividualCost num
  | num > 0 = Just (IndividualCost num)
  | otherwise = Nothing

unIndividualCost :: IndividualCost -> Scientific
unIndividualCost = coerce
