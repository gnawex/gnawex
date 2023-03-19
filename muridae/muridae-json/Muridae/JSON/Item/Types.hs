module Muridae.JSON.Item.Types
  ( module Muridae.JSON.Item.Types
  , module Muridae.JSON.Item.Id
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Vector (Vector)
import GHC.Generics (Generic)
import Muridae.JSON.Item.Id (ItemId (ItemId))
import Servant.API (HasStatus (StatusOf))
import qualified Muridae.JSON.PooledListing.Types as JSON

--------------------------------------------------------------------------------
-- Serialized Types

data Item = Item
  { id :: ItemId
  , name :: Text
  , description :: Text
  , wiki_link :: Text
  , created_at :: UTCTime
  , updated_at :: Maybe UTCTime
  , deleted_at :: Maybe UTCTime
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

-- | Contains the details to be shown in an item page.
--
-- The difference between this and @Item@ is that this contains the buy and sell
-- listings, except pooled.
data ItemDetails = ItemDetails
  { id :: ItemId
  -- ^ Item identifier
  , name :: Text
  -- ^ Name of the item
  , description :: Text
  -- ^ Description of an item
  , wiki_link :: Text
  -- ^ Link to MHWiki
  , pooled_buy_listings :: Vector JSON.PooledBuyListing
  -- ^ Pooled buy listings of item
  , pooled_sell_listings :: Vector JSON.PooledSellListing
  , created_at :: UTCTime
  -- ^ When the item was created
  , updated_at :: Maybe UTCTime
  -- ^ When the item was last updated
  , deleted_at :: Maybe UTCTime
  -- ^ Pooled sell listings of item
  -- TODO: Latest price
  -- , latest_transacted_cost :: Int32
  -- ^ How much this item was last transacted for
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

data ReqItem = ReqItem
  { name :: Text
  , description :: Text
  , wiki_link :: Text
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

--------------------------------------------------------------------------------
-- Handler Errors

--------------------------------------------------------------------------------
-- Instances

instance HasStatus Item where
  type StatusOf Item = 200

instance HasStatus [Item] where
  type StatusOf [Item] = 200

instance HasStatus (Vector Item) where
  type StatusOf (Vector Item) = 200

instance HasStatus ItemDetails where
  type StatusOf ItemDetails = 200
