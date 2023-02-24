{-# LANGUAGE FieldSelectors #-}

module Muridae.API.Route (module Muridae.API.Route) where

import GHC.Generics (Generic)
import Muridae.API.Route.Admin.Item qualified as AdminItem
import Muridae.API.Route.Item qualified as Item
import Muridae.API.Route.ItemListing qualified as ItemListing
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
  , itemListings :: mode :- "item_listings" :> ItemListing.Routes
  }
  deriving stock (Generic)

-- | Contains admin-specific routes
newtype AdminRoutes mode = AdminRoutes
  { items :: mode :- "items" :> AdminItem.Routes
  }
  deriving stock (Generic)
