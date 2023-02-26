module Muridae.API.Route.ItemListing (module Muridae.API.Route.ItemListing) where

import Data.Vector (Vector)
import GHC.Generics (Generic)
import Muridae.JSON.ItemListing.Types
  ( CreateItemListing
  , ItemListing
  , ItemListingCreate500
  , ItemListingId
  , ItemListingIndex500
  , ItemListingUpdate500
  , UpdateItemListing
  )
import Muridae.JSON.User (UserId)
import Servant.API
  ( Capture
  , Header
  , JSON
  , ReqBody
  , StdMethod (GET, PATCH, POST)
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
  , update
      :: mode
        :- Header "Current-User-Id" UserId
        :> Capture "item_listing_id" ItemListingId
        :> ReqBody '[JSON] UpdateItemListing
        :> UVerb
            'PATCH
            '[JSON]
            '[ ItemListing
             , WithStatus 201 ItemListing
            -- TODO: Use something more specific than @String@
             , WithStatus 401 String
             , WithStatus 404 String
             , WithStatus 500 ItemListingUpdate500
             ]
  }
  deriving stock (Generic)
