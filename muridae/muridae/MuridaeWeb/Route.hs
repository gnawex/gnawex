{-# LANGUAGE FieldSelectors #-}

module MuridaeWeb.Route (module MuridaeWeb.Route) where

import GHC.Generics (Generic)
import MuridaeWeb.Route.Admin.Item qualified as AdminItem
import MuridaeWeb.Route.Item qualified as Item
import MuridaeWeb.Route.ItemListing qualified as ItemListing
import Servant.API (type (:-), type (:>))
import Servant.API.NamedRoutes (NamedRoutes)

data APIv1 mode = APIv1
  { publicRoutes :: mode :- "api" :> "v1" :> NamedRoutes PublicRoutes
  , adminRoutes :: mode :- "api" :> "v1" :> "admin" :> NamedRoutes AdminRoutes
  }
  deriving stock (Generic)

-- | Contains guest and user routes
data PublicRoutes mode = PublicRoutes
  { items :: mode :- "items" :> Item.Routes
  , itemListings :: mode :- "listings" :> ItemListing.Routes
  }
  deriving stock (Generic)

-- | Contains admin-specific routes
newtype AdminRoutes mode = AdminRoutes
  { items :: mode :- "items" :> AdminItem.Routes
  }
  deriving stock (Generic)
