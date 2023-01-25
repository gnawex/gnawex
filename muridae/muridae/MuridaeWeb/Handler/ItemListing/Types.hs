module MuridaeWeb.Handler.ItemListing.Types
  ( ItemListing (..)
  , ItemListingId (..)
  , ItemListingType (..)
  , CreateItemListing (..)
  , PooledListing (..)
  , ResListingsUnderItem (..)
  , ReqStatus (..)
  , BatchedBy
  , UnitQuantity
  , Cost
  , mkBatchedBy
  , mkUnitQuantity
  , mkCost
  , mkBatchedBy'
  , mkUnitQuantity'
  , mkCost'
  , unBatchedBy
  , unUnitQuantity
  , unCost
  )
where

import Data.Aeson.Types
  ( FromJSON (parseJSON)
  , Parser
  , ToJSON
  , Value (Number)
  , prependFailure
  , typeMismatch
  )
import Data.Coerce (coerce)
import Data.Int (Int16, Int32)
import Data.Kind (Type)
import Data.Scientific (Scientific, toBoundedInteger)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import MuridaeWeb.Handler.Item.Types (ItemId)
import MuridaeWeb.Handler.User (UserId)
import Servant.API (FromHttpApiData)

newtype ItemListingId = ItemListingId Int32
  deriving (ToJSON, FromHttpApiData) via Int32
  deriving stock (Show)

data ItemListingType = BUY | SELL
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

data ItemListing = ItemListing
  { id :: ItemListingId
  , tradable_item_id :: ItemId
  , owner_id :: UserId
  , listing_type :: ItemListingType
  , batched_by :: BatchedBy
  , unit_quantity :: UnitQuantity
  , cost :: Cost
  , active :: Bool
  , created_at :: UTCTime
  , updated_at :: Maybe UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON)

data CreateItemListing = CreateItemListing
  { item_id :: ItemId
  , listing_type :: ItemListingType
  , batched_by :: BatchedBy
  , unit_quantity :: UnitQuantity
  , cost :: Cost
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

data PooledListing = PooledListing
  { cost :: Int32
  , batched_by :: Int16
  , unit_quantity :: Int32
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data ResListingsUnderItem = ResListingsUnderItem
  { buy :: [PooledListing]
  , sell :: [PooledListing]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

newtype ReqStatus = ReqStatus {active :: Bool}
  deriving stock (Generic)
  deriving anyclass (FromJSON)

newtype BatchedBy = BatchedBy Int16
  deriving (ToJSON) via Int16
  deriving stock (Show)

newtype UnitQuantity = UnitQuantity Int32
  deriving (ToJSON) via Int32
  deriving stock (Show)

newtype Cost = Cost Int32
  deriving (ToJSON) via Int32
  deriving stock (Show)

-------------------------------------------------------------------------------
-- Instances

instance FromJSON BatchedBy where
  parseJSON :: Value -> Parser BatchedBy
  parseJSON = \case
    Number batchedBy ->
      (maybe (failure' (Number batchedBy)) pure (parse' batchedBy))
    invalid -> failure' invalid
   where
    parse' :: Scientific -> Maybe BatchedBy
    parse' batchedBy = toBoundedInteger @Int16 batchedBy >>= mkBatchedBy

    failure' :: Value -> Parser BatchedBy
    failure' = failure "batched_by" "Int16"

instance FromJSON UnitQuantity where
  parseJSON :: Value -> Parser UnitQuantity
  parseJSON = \case
    Number qty ->
      (maybe (failure' (Number qty)) pure (parse' qty))
    invalid -> failure' invalid
   where
    parse' :: Scientific -> Maybe UnitQuantity
    parse' batchedBy = toBoundedInteger @Int32 batchedBy >>= mkUnitQuantity

    failure' :: Value -> Parser UnitQuantity
    failure' = failure "unit_quantity" "Int32"

instance FromJSON Cost where
  parseJSON :: Value -> Parser Cost
  parseJSON = \case
    Number qty ->
      (maybe (failure' (Number qty)) pure (parse' qty))
    invalid -> failure' invalid
   where
    parse' :: Scientific -> Maybe Cost
    parse' batchedBy = toBoundedInteger @Int32 batchedBy >>= mkCost

    failure' :: Value -> Parser Cost
    failure' = failure "cost" "Int32"

failure :: forall (a :: Type). String -> String -> Value -> Parser a
failure fieldName expectedType invalid =
  prependFailure
    (mconcat ["parsing '", fieldName, "' failed, "])
    (typeMismatch expectedType invalid)

-------------------------------------------------------------------------------
-- Smart constructors

mkBatchedBy :: Int16 -> Maybe BatchedBy
mkBatchedBy num
  | num > 0 = pure (BatchedBy num)
  | otherwise = Nothing

mkUnitQuantity :: Int32 -> Maybe UnitQuantity
mkUnitQuantity num
  | num > 0 = pure (UnitQuantity num)
  | otherwise = Nothing

mkCost :: Int32 -> Maybe Cost
mkCost num
  | num > 0 = pure (Cost num)
  | otherwise = Nothing

unBatchedBy :: BatchedBy -> Int16
unBatchedBy = coerce

unUnitQuantity :: UnitQuantity -> Int32
unUnitQuantity = coerce

unCost :: Cost -> Int32
unCost = coerce

mkBatchedBy' :: Int16 -> Either String BatchedBy
mkBatchedBy' num =
  maybe (Left "Failed to parse 'batched_by'") pure (mkBatchedBy num)

mkUnitQuantity' :: Int32 -> Either String UnitQuantity
mkUnitQuantity' num =
  maybe (Left "Failed to parse 'unit_quantity'") pure (mkUnitQuantity num)

mkCost' :: Int32 -> Either String Cost
mkCost' num =
  maybe (Left "Failed to parse 'cost'") pure (mkCost num)
