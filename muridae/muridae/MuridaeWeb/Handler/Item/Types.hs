module MuridaeWeb.Handler.Item.Types
  ( module MuridaeWeb.Handler.Item.Types
  )
where

import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Coerce (coerce)
import Data.Functor.Identity (Identity)
import Data.Int (Int32)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Muridae.Item.Types qualified as DB
import Muridae.ItemListing.Types qualified as DB
import MuridaeWeb.JSON.PooledListing (fromDbPooledBuyListing, fromDbPooledSellListing)
import MuridaeWeb.JSON.PooledListing qualified as JSON
import Servant.API (FromHttpApiData, HasStatus (StatusOf), ToHttpApiData)

newtype ItemId = ItemId Int32
  deriving stock (Show)
  deriving (ToJSON, FromJSON, FromHttpApiData, ToHttpApiData, Eq) via Int32

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

instance HasStatus ItemDetails where
  type StatusOf ItemDetails = 200

--------------------------------------------------------------------------------

fromItem
  :: (DB.Item Identity, [DB.PooledBuyListing], [DB.PooledSellListing])
  -> ItemDetails
fromItem (item, pooledBuys, pooledSells) =
  ItemDetails
    { id = coerce item._id
    , name = item._name
    , description = item._description
    , wiki_link = item._wiki_link
    , pooled_buy_listings = fromDbPooledBuyListing <$> pooledBuys
    , pooled_sell_listings = fromDbPooledSellListing <$> pooledSells
    }
