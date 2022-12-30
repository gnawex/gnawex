module MuridaeWeb.Route.ItemListing where

import GHC.Generics (Generic)
import MuridaeWeb.Handler.Item.Listing.Types (
  CreateTradableItemListing,
  TradableItemListing,
 )
import MuridaeWeb.Handler.Item.Types (TradableItemId)
import Servant (JSON)
import Servant.API (NoContent, ReqBody, type (:>), Header)
import Servant.API.Capture (Capture)
import Servant.API.Generic (type (:-))
import Servant.API.NamedRoutes (NamedRoutes)
import Servant.API.Verbs (Get, PostCreated)
import MuridaeWeb.Handler.User (UserId)

type Routes = NamedRoutes Routes'

data Routes' mode = Routes'
  { index ::
      mode
        :- Capture "item_id" TradableItemId
        :> "listings"
        :> Get '[JSON] [TradableItemListing]
  , create ::
      mode
        :- Header "Current-User-Id" UserId
        :> Capture "item_id" TradableItemId
        :> "listings"
        :> ReqBody '[JSON] CreateTradableItemListing
        :> PostCreated '[JSON] NoContent
  }
  deriving stock (Generic)
