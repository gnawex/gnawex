module MuridaeWeb.Handler.ItemListing.Types
  ( module MuridaeWeb.Handler.ItemListing.Types
  )
where

import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Int (Int16, Int32)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import MuridaeWeb.Handler.User (UserId)
import MuridaeWeb.JSON.Item (ItemId)
import Servant.API
  ( FromHttpApiData (parseQueryParam)
  , HasStatus (StatusOf)
  , ToHttpApiData (toQueryParam)
  )

newtype ItemListingId = ItemListingId Int32
  deriving stock (Show, Eq)
  deriving (ToJSON, FromHttpApiData, ToHttpApiData, FromJSON) via Int32

data ItemListingType = BUY | SELL
  deriving stock (Eq, Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

data ItemListing = ItemListing
  { id :: ItemListingId
  , tradable_item_id :: ItemId
  , owner_id :: UserId
  , listing_type :: ItemListingType
  , batched_by :: Int16
  , unit_quantity :: Int32
  , cost :: Int32
  , active :: Bool
  , created_at :: UTCTime
  , updated_at :: Maybe UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

data CreateItemListing = CreateItemListing
  { item_id :: ItemId
  , listing_type :: ItemListingType
  , batched_by :: Int16
  , unit_quantity :: Int32
  , cost :: Int32
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype ReqStatus = ReqStatus {active :: Bool}
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

--------------------------------------------------------------------------------
-- Instances

instance HasStatus ItemListing where
  type StatusOf ItemListing = 200

instance FromHttpApiData ItemListingType where
  parseQueryParam :: Text -> Either Text ItemListingType
  parseQueryParam = \case
    "BUY" -> pure BUY
    "SELL" -> pure SELL
    invalid ->
      Left $
        mconcat
          [ "Invalid value `"
          , invalid
          , "` for item listing type."
          , " I expected `BUY` or `SELL`"
          ]

instance ToHttpApiData ItemListingType where
  toQueryParam = \case
    BUY -> "BUY"
    SELL -> "SELL"
