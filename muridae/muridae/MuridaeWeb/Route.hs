module MuridaeWeb.Route (
  NamedAPI,
  API (..),
  PublicRoutes (..),
  AdminRoutes (..),
  api,
) where

import Data.Data (Proxy (Proxy))
import GHC.Generics (Generic)
import qualified MuridaeWeb.Route.Admin.Item as AdminItem
import qualified MuridaeWeb.Route.Item as Item
import Servant.API (type (:-), type (:>))
import Servant.API.Generic (ToServantApi, genericApi)
import Servant.API.NamedRoutes (NamedRoutes)

type NamedAPI = NamedRoutes API

data API mode = API
  { publicRoutes :: mode :- "api" :> NamedRoutes PublicRoutes
  , adminRoutes :: mode :- "api" :> "admin" :> NamedRoutes AdminRoutes
  }
  deriving stock (Generic)

data PublicRoutes mode = PublicRoutes
  { items :: mode :- "items" :> Item.Routes
  }
  deriving stock (Generic)

data AdminRoutes mode = AdminRoutes
  { items :: mode :- "items" :> AdminItem.Routes
  }
  deriving stock (Generic)

api :: Proxy (ToServantApi API)
api = genericApi (Proxy :: Proxy API)
