{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Muridae.Listing where

import Effectful (Eff, Effect, type (:>))
import Effectful.Beam (DB, queryDebug)
import qualified DB.Types as DB
import qualified DB.TradableItemListings as TradableItemListing
import Data.Functor.Identity (Identity)

all :: forall (es :: [Effect]). (DB :> es) => Eff es [DB.TradableItemListing Identity]
all = queryDebug print TradableItemListing.all

-- create ::
--   forall (es :: [Effect]).
--   DB :> es =>
--   -- User ID
--   Int ->
--   -- Item ID
--   Int ->
--   -- Batched By
--   Int ->
--   -- Quantity
--   Int ->
--   -- Cost
--   Int ->
--   Eff es ()
-- create userId itemId batchedBy unitQuantity cost = _
