module MuridaeWeb.Route.Admin.Item where

import GHC.Generics (Generic)
import MuridaeWeb.Handler.Item.Types (ReqTradableItem, TradableItem)
import Servant.API (GenericMode (type (:-)), NamedRoutes, ReqBody, type (:>))
import Servant.API.ContentTypes (JSON)
import Servant.API.Verbs (Get, PostCreated)
import Servant.API.ContentTypes (NoContent)

type Routes = NamedRoutes Routes'

data Routes' mode = Routes'
  { index :: mode :- Get '[JSON] [TradableItem]
  , create ::
      mode
        :- ReqBody '[JSON] ReqTradableItem
        :> PostCreated '[JSON] NoContent
  }
  deriving stock (Generic)
