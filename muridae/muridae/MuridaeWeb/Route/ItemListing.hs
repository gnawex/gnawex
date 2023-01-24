module MuridaeWeb.Route.ItemListing (module MuridaeWeb.Route.ItemListing) where

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
  , type (:>)
  )
import Servant.API.Generic (type (:-))
import Servant.API.NamedRoutes (NamedRoutes)
import Servant.API.Verbs (Get, Patch, PostCreated)

type Routes = NamedRoutes Routes'

data Routes' mode = Routes'
  { index :: mode :- Get '[JSON] [ItemListing]
  , create
      :: mode
        :- Header "Current-User-Id" UserId
        :> ReqBody '[JSON] CreateItemListing
        :> PostCreated '[JSON] NoContent
  , updateStatus
      :: mode
        :- Header "Current-User-Id" UserId
        :> Capture "item_listing_id" ItemListingId
        :> "status"
        :> ReqBody '[JSON] ReqStatus
        :> Patch '[JSON] ItemListing
  }
  deriving stock (Generic)
