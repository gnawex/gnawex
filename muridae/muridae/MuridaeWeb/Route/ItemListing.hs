module MuridaeWeb.Route.ItemListing where

import GHC.Generics (Generic)
import Servant (JSON)
import Servant.API.Generic (type (:-))
import Servant.API.NamedRoutes (NamedRoutes)
import Servant.API.Verbs (Get)
import MuridaeWeb.Handler.Item.Listing.Types (TradableItemListing)
import Servant.API.Capture (Capture)
import MuridaeWeb.Handler.Item.Types (TradableItemId)
import Servant.API (type (:>))

type Routes = NamedRoutes Routes'

data Routes' mode = Routes'
  { index ::
      mode
        :- Capture "item_id" TradableItemId
        :> "listings"
        :> Get '[JSON] [TradableItemListing]
  }
  deriving stock (Generic)
