module MuridaeWeb.Route.Item where

import GHC.Generics (Generic)
import MuridaeWeb.Handler.Item.Types (TradableItem, TradableItemId)
import MuridaeWeb.Handler.ItemListing.Types (ResListingsUnderItem)
import Servant (Capture, JSON, type (:>))
import Servant.API.Generic (type (:-))
import Servant.API.NamedRoutes (NamedRoutes)
import Servant.API.Verbs (Get)

type Routes = NamedRoutes Routes'

data Routes' mode = Routes'
    { index :: mode :- Get '[JSON] [TradableItem]
    , getListingsUnderItem
        :: mode
            :- Capture "item_id" TradableItemId
                :> "listings"
                :> Get '[JSON] ResListingsUnderItem
    }
    deriving stock (Generic)
