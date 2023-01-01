module MuridaeWeb.Handler.Item where

import Muridae.Item qualified as Item
import MuridaeWeb.Handler.Item.Types (TradableItem)
import MuridaeWeb.Handler.Item.Types qualified as Handler
import MuridaeWeb.Types (Handler')
import Servant.API.ContentTypes (NoContent (NoContent))

create :: Handler.ReqTradableItem -> Handler' NoContent
create params = Item.create_ params >> pure NoContent

indexItems :: Handler' [TradableItem]
indexItems = Item.list
