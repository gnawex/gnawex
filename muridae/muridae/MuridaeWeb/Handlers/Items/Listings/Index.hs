{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}

module MuridaeWeb.Handlers.Items.Listings.Index where

import qualified DB.TradableItemListings as DB.TradableItemListing

import qualified DB.Types as DB
import Data.Aeson.Types (ToJSON)
import Data.Coerce (coerce)
import Data.Functor.Identity (Identity)
import Data.Int (Int16, Int32, Int64)
import Data.Time (UTCTime)
import Effectful (liftIO)
import Effectful.Beam (queryDebug)
import GHC.Generics (Generic)
import MuridaeWeb.Handlers.Items.Types (TradableItemId (TradableItemId))
import MuridaeWeb.Types (Handler')

newtype TradableItemListingId = TradableListingId Int32
  deriving (ToJSON) via Int32

data TradableItemListingType = BUY | SELL
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data TradableItemListing = TradableItemListing
  { id :: TradableItemListingId
  , tradable_item_id :: TradableItemId
  , listing_type :: TradableItemListingType
  , batched_by :: Int16
  , unit_quantity :: Int32
  , cost :: Int64
  , active :: Bool
  , created_at :: UTCTime
  , updated_at :: Maybe UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

-- | Get all the listings under a tradable item
index :: TradableItemId -> Handler' [TradableItemListing]
index _ =
  queryDebug print DB.TradableItemListing.all
    >>= \listings -> do
      liftIO $ print listings

      pure . (fmap parseDBItemListing) $ listings
 where
  parseDBItemListing :: DB.TradableItemListing Identity -> TradableItemListing
  parseDBItemListing dbItemListing =
    TradableItemListing
      { id = coerce dbItemListing._id
      , tradable_item_id = coerce dbItemListing._tradable_item
      , listing_type = fromDbListingType dbItemListing._type
      , batched_by = dbItemListing._batched_by
      , unit_quantity = dbItemListing._unit_quantity
      , cost = dbItemListing._cost
      , active = dbItemListing._active
      , created_at = dbItemListing._created_at
      , updated_at = dbItemListing._updated_at
      }

  fromDbListingType dbListingType =
    case dbListingType of
      DB.Buy -> BUY
      DB.Sell -> SELL
