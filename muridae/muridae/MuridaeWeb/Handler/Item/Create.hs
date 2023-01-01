module MuridaeWeb.Handler.Item.Create where

import Effectful.Beam (queryDebug)
import MuridaeWeb.Handler.Item.Types qualified as Handler
import MuridaeWeb.Types (Handler')
import Servant.API.ContentTypes (NoContent (NoContent))
import Muridae.Item.Model qualified as ItemModel

create :: Handler.ReqTradableItem -> Handler' NoContent
create reqItem =
    queryDebug print (ItemModel.create reqItem) >> pure NoContent
