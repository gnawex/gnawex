module MuridaeWeb.Handler.Item (module MuridaeWeb.Handler.Item) where

import Muridae.Item qualified as Item
import MuridaeWeb.Handler.Item.Types (Item)
import MuridaeWeb.Handler.Item.Types qualified as Handler
import MuridaeWeb.Types (Handler')
import Servant.API.ContentTypes (NoContent (NoContent))

create :: Handler.ReqItem -> Handler' NoContent
create params = Item.create_ params >> pure NoContent

indexItems :: Handler' [Item]
indexItems = Item.list
