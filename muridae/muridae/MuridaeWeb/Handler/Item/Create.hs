module MuridaeWeb.Handler.Item.Create where

import Effectful.Beam (queryDebug)
import Muridae.Model.TradableItem qualified as DB.TradableItem
import MuridaeWeb.Handler.Item.Types qualified as Handler
import MuridaeWeb.Types (Handler')
import Servant.API.ContentTypes (NoContent (NoContent))

create :: Handler.ReqTradableItem -> Handler' NoContent
create reqItem =
    queryDebug print (DB.TradableItem.create reqItem) >> pure NoContent
