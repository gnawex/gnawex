module MuridaeWeb.Route.ItemListing (module MuridaeWeb.Route.ItemListing) where

import Effectful.Beam (DbError)
import GHC.Generics (Generic)
import MuridaeWeb.Handler.ItemListing.Types
  ( CreateItemListing
  , ItemListing
  , ItemListingId
  , ReqStatus
  )
import MuridaeWeb.Handler.User (UserId)
import Servant (JSON)
import Servant.API
  ( Capture
  , Header
  , NoContent
  , ReqBody
  , StdMethod (GET, PATCH, POST)
  , UVerb
  , WithStatus
  , type (:>)
  )
import Servant.API.Generic (type (:-))
import Servant.API.NamedRoutes (NamedRoutes)

-- TODO: Make response types for common unauthorized/404 things

type Routes = NamedRoutes Routes'

data Routes' mode = Routes'
  { index
      :: mode
        :- UVerb
            'GET
            '[JSON]
            '[ WithStatus 200 [ItemListing]
             , WithStatus 500 DbError
             ]
  , create
      :: mode
        :- Header "Current-User-Id" UserId
        :> ReqBody '[JSON] CreateItemListing
        :> UVerb
            'POST
            '[JSON]
            '[ NoContent
             , WithStatus 401 String
             , WithStatus 500 DbError
             ]
  , updateStatus
      :: mode
        :- Header "Current-User-Id" UserId
        :> Capture "item_listing_id" ItemListingId
        :> "status"
        :> ReqBody '[JSON] ReqStatus
        :> UVerb
            'PATCH
            '[JSON]
            '[ ItemListing
             , WithStatus 401 String
             , WithStatus 404 String
             , WithStatus 500 DbError
             ]
  }
  deriving stock (Generic)
