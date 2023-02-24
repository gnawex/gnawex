module Muridae.JSON.ItemListing.Types
  ( module Muridae.JSON.ItemListing.Types
  )
where

import Data.Aeson (ToJSON (toJSON), object, FromJSON, Value (String))
import Data.Int (Int16, Int32, Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Hasql.Pool (DbError)
import Muridae.ItemListing.Types (ItemListingParseError)
import Muridae.JSON.Item.Id (ItemId)
import Muridae.JSON.User (User, UserId)
import Servant.API
  ( FromHttpApiData (parseQueryParam)
  , ToHttpApiData (toQueryParam)
  )

newtype ItemListingId = ItemListingId Int64
  deriving stock (Show, Eq)
  deriving (ToJSON, FromHttpApiData, ToHttpApiData, FromJSON) via Int64

data ItemListingType = BUY | SELL
  deriving stock (Eq, Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

data ItemListing = ItemListing
  { id :: ItemListingId
  , tradable_item_id :: ItemId
  , owner_id :: UserId
  , owner :: User
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
-- Error responses

data ItemListingIndex500
  = DbError DbError
  | ParseError ItemListingParseError
  deriving stock (Eq, Show)

instance ToJSON ItemListingIndex500 where
  toJSON :: ItemListingIndex500 -> Value
  toJSON = \case
    DbError _dbError ->
      object [("message", String "DB ded")]
    ParseError _err ->
      object [("message", String "Parse failed")]

--------------------------------------------------------------------------------
-- Instances

-- instance HasStatus ItemListing where
--   type StatusOf ItemListing = 200

-- instance HasStatus (Vector ItemListing) where
--   type StatusOf (Vector ItemListing) = 200

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
