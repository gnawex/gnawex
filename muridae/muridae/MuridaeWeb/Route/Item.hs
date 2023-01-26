module MuridaeWeb.Route.Item (module MuridaeWeb.Route.Item) where

import Effectful.Beam (DbError)
import GHC.Generics (Generic)
import MuridaeWeb.Handler.Item.Types (Item, ItemId)
import MuridaeWeb.Handler.ItemListing.Types (ResListingsUnderItem)
import Servant (Capture, JSON, UVerb, WithStatus, type (:>))
import Servant.API.Generic (type (:-))
import Servant.API.NamedRoutes (NamedRoutes)
import Servant.API.Verbs (StdMethod (GET))

type Routes = NamedRoutes Routes'

data Routes' mode = Routes'
  { index
      :: mode
        :- UVerb
            'GET
            '[JSON]
            '[ [Item]
             , WithStatus 500 DbError
             ]
  , getListingsUnderItem
      :: mode
        :- Capture "item_id" ItemId
        :> "listings"
        :> UVerb
            'GET
            '[JSON]
            '[ ResListingsUnderItem
             , WithStatus 500 DbError
             ]
  }
  deriving stock (Generic)
