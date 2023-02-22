module MuridaeWeb.Route.Admin.Item (module MuridaeWeb.Route.Admin.Item) where

import GHC.Generics (Generic)
import MuridaeWeb.JSON.DbError (DbError)
import MuridaeWeb.JSON.Item (ReqItem, Item)
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

data Routes' mode = Routes'
  { create
      :: mode
        :- ReqBody '[JSON] ReqItem
        :> UVerb 'POST '[JSON] '[WithStatus 201 Item, DbError]
  }
  deriving stock (Generic)
