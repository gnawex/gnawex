{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module MuridaeWeb.Handlers.Items.Create where

import Data.Pool (withResource)
import qualified Database.Beam.Postgres as Beam
import Effectful (liftIO)
import Effectful.Reader.Static (asks)
import qualified Muridae.DB.TradableItem as DB.TradableItem
import Muridae.Environment (pool)
import qualified MuridaeWeb.Handlers.Items.Types as Handler
import MuridaeWeb.Types (Handler')
import Servant.API.ContentTypes (NoContent (NoContent))

create :: Handler.ReqTradableItem -> Handler' NoContent
create reqItem = do
  connPool <- asks pool

  liftIO $ withResource connPool $ \conn -> do
    Beam.runBeamPostgresDebug print conn (DB.TradableItem.create reqItem)

  pure NoContent
