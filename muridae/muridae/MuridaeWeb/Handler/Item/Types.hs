module MuridaeWeb.Handler.Item.Types where

import Data.Aeson.Types (ToJSON, FromJSON)
import Data.Int (Int32)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Servant.API (FromHttpApiData)

newtype TradableItemId = TradableItemId Int32
  deriving (ToJSON, FromHttpApiData) via Int32

data TradableItem = TradableItem
  { id :: TradableItemId
  , name :: Text
  , description :: Text
  , wiki_link :: Text
  , created_at :: UTCTime
  , updated_at :: Maybe UTCTime
  , deleted_at :: Maybe UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data ReqTradableItem = ReqTradableItem
  { name :: Text
  , description :: Text
  , wiki_link :: Text
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)
