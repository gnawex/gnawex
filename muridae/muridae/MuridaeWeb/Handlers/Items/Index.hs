{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module MuridaeWeb.Handlers.Items.Index where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON)
import Data.Coerce (coerce)
import Data.Int (Int32)
import Data.Pool (withResource)
import Data.Text (Text)
import Data.Time (UTCTime)
import qualified Database.Beam.Postgres as Beam
import Effectful.Reader.Static (asks)
import GHC.Generics (Generic)
import qualified Muridae.DB.TradableItem as TradableItem
import Muridae.Environment (pool)
import MuridaeWeb.Types (Handler')

newtype ItemId = ItemId Int32
  deriving (ToJSON) via Int32

data Item = Item
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

indexItems :: Handler' [Item]
indexItems = do
  connPool <- asks pool

  liftIO $ withResource connPool $ \conn -> do
    items <- Beam.runBeamPostgresDebug print conn TradableItem.getTradableItems

    pure $ parseDBItem <$> items
 where
  parseDBItem dbItem =
    Item
      { id = coerce dbItem._id
      , name = dbItem._name
      , description = dbItem._description
      , wiki_link = dbItem._wiki_link
      , created_at = dbItem._created_at
      , updated_at = dbItem._updated_at
      , deleted_at = dbItem._deleted_at
      }
