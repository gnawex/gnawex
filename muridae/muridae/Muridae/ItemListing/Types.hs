module Muridae.ItemListing.Types
  ( PooledBuyListing (..)
  , PooledSellListing (..)
  , Cost
  , BatchedBy
  , UnitQuantity
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
