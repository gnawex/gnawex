module Muridae.API.Route.Admin.Item (module Muridae.API.Route.Admin.Item) where

import GHC.Generics (Generic)
import Hasql.Pool (DbError)
import Muridae.JSON.Item.Types (Item, ReqItem)
import Servant.API
  ( GenericMode (type (:-))
  , NamedRoutes
  , ReqBody
  , StdMethod (POST)
  , UVerb
  , WithStatus
  , type (:>)
  )
import Servant.API.ContentTypes (JSON)

type Routes = NamedRoutes Routes'

newtype Routes' mode = Routes'
  { create
      :: mode
        :- ReqBody '[JSON] ReqItem
        :> UVerb 'POST '[JSON] '[WithStatus 201 Item, WithStatus 500 DbError]
  }
  deriving stock (Generic)
