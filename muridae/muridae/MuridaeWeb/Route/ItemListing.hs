module MuridaeWeb.Route.ItemListing where

import GHC.Generics (Generic)
import MuridaeWeb.Handler.ItemListing.Types (
  CreateTradableItemListing,
  TradableItemListing,
 )
import MuridaeWeb.Handler.User (UserId)
import Servant (JSON)
import Servant.API (Header, NoContent, ReqBody, type (:>))
import Servant.API.Generic (type (:-))
import Servant.API.NamedRoutes (NamedRoutes)
import Servant.API.Verbs (Get, PostCreated)

type Routes = NamedRoutes Routes'

data Routes' mode = Routes'
  { index :: mode :- Get '[JSON] [TradableItemListing]
  , create ::
      mode
        :- Header "Current-User-Id" UserId
        :> ReqBody '[JSON] CreateTradableItemListing
        :> PostCreated '[JSON] NoContent
  }
  deriving stock (Generic)
