{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}

module MuridaeWeb.Handlers.Items.Types where

import Data.Aeson.Types (ToJSON, FromJSON)
import Data.Int (Int32)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

newtype ItemId = ItemId Int32
  deriving (ToJSON) via Int32

data TradableItem = TradableItem
  { id :: ItemId
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
