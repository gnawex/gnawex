module MuridaeWeb.Route.Item (module MuridaeWeb.Route.Item) where

import Effectful.Beam (DbError)
import GHC.Generics (Generic)
import MuridaeWeb.Handler.Item.Types (Item, ItemDetails, ItemId)
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
  , show
      :: mode
        :- Capture "item_id" ItemId
        :> UVerb
            'GET
            '[JSON]
            '[ WithStatus 200 ItemDetails
             , WithStatus 404 String
             , WithStatus 500 DbError
             ]
  -- TODO: Replace with an endpoint that responds with listings, not pooled,
  -- with filters for listing type, and ordering.
  -- , getListingsUnderItem
  --     :: mode
  --       :- Capture "item_id" ItemId
  --       :> "listings"
  --       :> UVerb
  --           'GET
  --           '[JSON]
  --           '[ ResListingsUnderItem
  --            , WithStatus 500 DbError
  --            ]
  }
  deriving stock (Generic)
