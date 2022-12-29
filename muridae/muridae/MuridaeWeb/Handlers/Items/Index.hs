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
import Data.Coerce (coerce)
import Data.Functor.Identity (Identity)
import Data.Pool (withResource)
import qualified Database.Beam.Postgres as Beam
import Database.PostgreSQL.Simple.Transaction (withTransactionSerializable)
import Effectful.Reader.Static (asks)
import qualified Muridae.DB.TradableItem as DB.TradableItem
import qualified Muridae.DB.Types as DB
import Muridae.Environment (pool)
import qualified MuridaeWeb.Handlers.Items.Types as Handler
import MuridaeWeb.Types (Handler')

indexItems :: Handler' [Handler.TradableItem]
indexItems = do
  connPool <- asks pool

  liftIO $
    withResource connPool $
      \conn -> withTransactionSerializable conn $ do
        items <- Beam.runBeamPostgresDebug print conn DB.TradableItem.all

        pure $ parseDBItem <$> items
 where
  parseDBItem :: DB.TradableItem Identity -> Handler.TradableItem
  parseDBItem dbItem =
    Handler.TradableItem
      { id = coerce dbItem._id
      , name = dbItem._name
      , description = dbItem._description
      , wiki_link = dbItem._wiki_link
      , created_at = dbItem._created_at
      , updated_at = dbItem._updated_at
      , deleted_at = dbItem._deleted_at
      }
