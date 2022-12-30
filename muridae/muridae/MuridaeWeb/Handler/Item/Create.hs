module MuridaeWeb.Handler.Item.Create where

import Effectful.Beam (queryDebug)
import qualified DB.TradableItem as DB.TradableItem
import qualified MuridaeWeb.Handler.Item.Types as Handler
import MuridaeWeb.Types (Handler')
import Servant.API.ContentTypes (NoContent (NoContent))

create :: Handler.ReqTradableItem -> Handler' NoContent
create reqItem =
  queryDebug print (DB.TradableItem.create reqItem) >> pure NoContent
