module Muridae.API.Route.ItemListing (module Muridae.API.Route.ItemListing) where

import Data.Vector (Vector)
import GHC.Generics (Generic)
import Muridae.JSON.ItemListing.Types
  ( CreateItemListing
  , ItemListing
  , ItemListingIndex500, ItemListingCreate500
  )
import Muridae.JSON.User (UserId)
import Servant.API
  ( Header
  , JSON
  , ReqBody
  , StdMethod (GET, POST)
  , UVerb
  , WithStatus
  , (:>)
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
  , create
      :: mode
        :- Header "Current-User-Id" UserId
        :> ReqBody '[JSON] CreateItemListing
        :> UVerb
            'POST
            '[JSON]
            '[ ItemListing
             , WithStatus 401 String
             , WithStatus 500 ItemListingCreate500
             ]
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
