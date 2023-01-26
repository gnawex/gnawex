module MuridaeWeb.Route.Admin.Item (module MuridaeWeb.Route.Admin.Item) where

import Effectful.Beam (DbError)
import GHC.Generics (Generic)
import MuridaeWeb.Handler.Item.Types (Item, ReqItem)
import Servant.API
  ( GenericMode (type (:-))
  , NamedRoutes
  , ReqBody
  , StdMethod (GET, POST)
  , UVerb
  , WithStatus
  , type (:>)
  )
import Servant.API.ContentTypes (JSON, NoContent)

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
  , create
      :: mode
        :- ReqBody '[JSON] ReqItem
        :> UVerb 'POST '[JSON] '[WithStatus 201 NoContent, WithStatus 500 DbError]
  }
  deriving stock (Generic)
