{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module MuridaeWeb.Handlers.Items.Index where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON)
import Data.Int (Int32)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Time.Clock (getCurrentTime)
import GHC.Generics (Generic)
import MuridaeWeb.Types (Handler')

newtype ItemId = ItemId Int32
  deriving (ToJSON) via Int32

data Item = Item
  { id :: ItemId
  , name :: Text
  , description :: Text
  , wiki_link :: Text
  , created_at :: UTCTime
  , updated_at :: UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

indexItems :: Handler' [Item]
indexItems = do
  timeNow <- liftIO getCurrentTime

  pure
    [ Item
        { id = ItemId 1
        , name = "Hey"
        , description = "Bruh"
        , wiki_link = "https://mousehuntgame.com"
        , created_at = timeNow
        , updated_at = timeNow
        }
    ]
