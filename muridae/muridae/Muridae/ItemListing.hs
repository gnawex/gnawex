module Muridae.ItemListing
  ( serializePooledBuys
  , serializePooledSells
  )
where

import Data.Int (Int16, Int32, Int64)
import Muridae.ItemListing.Types
  ( PooledBuyListing (PooledBuyListing)
  , PooledSellListing (PooledSellListing)
  , mkBatchedBy
  , mkCost
  , mkUnitQuantity', mkIndividualCost
  )
import Data.Scientific (Scientific)

serializePooledBuys :: (Int32, Int16, Int64, Scientific) -> Maybe PooledBuyListing
serializePooledBuys (cost, batchedBy, summedUnitQuantity, individualCost) =
  pure PooledBuyListing
    <*> mkCost cost
    <*> mkBatchedBy batchedBy
    <*> mkUnitQuantity' summedUnitQuantity
    <*> mkIndividualCost individualCost

serializePooledSells :: (Int32, Int16, Int64, Scientific) -> Maybe PooledSellListing
serializePooledSells (cost, batchedBy, summedUnitQuantity, individualCost) =
  pure PooledSellListing
    <*> mkCost cost
    <*> mkBatchedBy batchedBy
    <*> mkUnitQuantity' summedUnitQuantity
    <*> mkIndividualCost individualCost
