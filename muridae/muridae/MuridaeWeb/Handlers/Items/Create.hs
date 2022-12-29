{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module MuridaeWeb.Handlers.Items.Create where

import Effectful.Beam (queryDebug)
import qualified DB.TradableItem as DB.TradableItem
import qualified MuridaeWeb.Handlers.Items.Types as Handler
import MuridaeWeb.Types (Handler')
import Servant.API.ContentTypes (NoContent (NoContent))

create :: Handler.ReqTradableItem -> Handler' NoContent
create reqItem =
  queryDebug print (DB.TradableItem.create reqItem) >> pure NoContent
