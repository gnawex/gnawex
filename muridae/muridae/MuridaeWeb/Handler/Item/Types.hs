module MuridaeWeb.Handler.Item.Types
  ( module MuridaeWeb.Handler.Item.Types
  )
where

import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Muridae.Item.Types qualified as Domain
import MuridaeWeb.JSON.PooledListing qualified as JSON
import Servant.API (FromHttpApiData, HasStatus (StatusOf), ToHttpApiData)
import Data.Vector (Vector)

newtype ItemId = ItemId Int64
  deriving stock (Show)
  deriving (ToJSON, FromJSON, FromHttpApiData, ToHttpApiData, Eq) via Int64

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
  , pooled_buy_listings :: [JSON.PooledBuyListing]
  -- ^ Pooled buy listings of item
  , pooled_sell_listings :: [JSON.PooledSellListing]
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
-- Instances

instance HasStatus Item where
  type StatusOf Item = 200

instance HasStatus [Item] where
  type StatusOf [Item] = 200

instance HasStatus (Vector Item) where
  type StatusOf (Vector Item) = 200

instance HasStatus ItemDetails where
  type StatusOf ItemDetails = 200

-------------------------------------------------------------------------------

-- | Serializes an @Item@
parseItem :: Domain.Item -> Item
parseItem (Domain.Item (Domain.ItemId itemId) name desc wiki createdAt updatedAt deletedAt) =
  Item (ItemId itemId) name desc wiki createdAt updatedAt deletedAt
