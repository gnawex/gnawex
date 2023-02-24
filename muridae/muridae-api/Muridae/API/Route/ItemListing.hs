module Muridae.API.Route.ItemListing (module Muridae.API.Route.ItemListing) where

import Data.Vector (Vector)
import GHC.Generics (Generic)
import Muridae.JSON.ItemListing.Types
  ( ItemListing
  , ItemListingIndex500
  )
import Servant (JSON)
import Servant.API
  ( StdMethod (GET)
  , UVerb
  , WithStatus
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
            '[ WithStatus 200 (Vector ItemListing)
             , WithStatus 500 ItemListingIndex500
             ]
             -- , create
             --     :: mode
             --       :- Header "Current-User-Id" UserId
             --       :> ReqBody '[JSON] CreateItemListing
             --       :> UVerb
             --           'POST
             --           '[JSON]
             --           '[ NoContent
             --            , WithStatus 401 String
             --            , WithStatus 500 DbError
             --            ]
             -- , updateStatus
             --     :: mode
             --       :- Header "Current-User-Id" UserId
             --       :> Capture "item_listing_id" ItemListingId
             --       :> "status"
             --       :> ReqBody '[JSON] ReqStatus
             --       :> UVerb
             --           'PATCH
             --           '[JSON]
             --           '[ WithStatus 200 ItemListing
             --            , WithStatus 401 String
             --            , WithStatus 404 String
             --            , WithStatus 500 DbError
             --            ]
  }
  deriving stock (Generic)
