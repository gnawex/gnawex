module Muridae.JSON.ItemListing.Types
  ( module Muridae.JSON.ItemListing.Types
  )
where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value (Number, Object, String), object, (.:))
import Data.Aeson.Types (Parser, prependFailure, typeMismatch)
import Data.Int (Int16, Int32, Int64)
import Data.Scientific (scientific)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Vector (Vector)
import GHC.Generics (Generic)
import Muridae.ItemListing.Types (ItemListingParseError)
import Muridae.JSON.DbError (DbError)
import Muridae.JSON.Item.Id (ItemId)
import Muridae.JSON.User (User, UserId)
import Servant.API
  ( FromHttpApiData (parseQueryParam)
  , HasStatus (StatusOf)
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
  , item_id :: ItemId
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
  deriving anyclass (ToJSON)

newtype ReqStatus = ReqStatus {active :: Bool}
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

--------------------------------------------------------------------------------
-- Error responses

data ItemListingIndex500
  = IndexDbError DbError
  | IndexParseError ItemListingParseError
  deriving stock (Eq, Show)

data ItemListingCreate500
  = CreateDbError DbError
  | CreateParseError ItemListingParseError
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Instances

-- instance FromJSON CreateItemListing where
--   parseJSON :: Value -> Parser CreateItemListing
--   parseJSON = _

instance FromJSON CreateItemListing where
  parseJSON = \case
    Object v ->
      CreateItemListing
        <$> (v .: "item_id")
        <*> (v .: "listing_type")
        <*> parseNonZeroNum "batched_by" (v .: "batched_by")
        <*> parseNonZeroNum "unit_quantity" (v .: "unit_quantity")
        <*> parseNonZeroNum "cost" (v .: "cost")
    invalid ->
      prependFailure "parsing request failed" (typeMismatch "Object" invalid)
   where
    parseNonZeroNum :: Integral a => String -> Parser a -> Parser a
    parseNonZeroNum field pNum =
      pNum
        >>= \num ->
          if num > 0
            then pure num
            else
              prependFailure
                "parsing request failed, "
                ( typeMismatch
                    ("Non-zero integer for " <> field)
                    (Number (scientific (fromIntegral num) 1))
                )

instance ToJSON ItemListingCreate500 where
  toJSON :: ItemListingCreate500 -> Value
  toJSON = \case
    CreateDbError dbError -> toJSON dbError
    CreateParseError _err ->
      object [("message", String "Parse failed")]

instance ToJSON ItemListingIndex500 where
  toJSON :: ItemListingIndex500 -> Value
  toJSON = \case
    IndexDbError dbError -> toJSON dbError
    IndexParseError _err ->
      object [("message", String "Parse failed")]

instance HasStatus ItemListing where
  type StatusOf ItemListing = 200

instance HasStatus (Vector ItemListing) where
  type StatusOf (Vector ItemListing) = 200

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
