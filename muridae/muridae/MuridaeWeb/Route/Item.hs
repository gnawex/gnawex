{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators #-}

module MuridaeWeb.Route.Item where

import GHC.Generics (Generic)
import MuridaeWeb.Handler.Item.Types (TradableItem)
import qualified MuridaeWeb.Route.ItemListing as ItemListings
import Servant (JSON)
import Servant.API.Generic (type (:-))
import Servant.API.NamedRoutes (NamedRoutes)
import Servant.API.Verbs (Get)

type Routes = NamedRoutes Routes'

data Routes' mode = Routes'
  { indexItems :: mode :- Get '[JSON] [TradableItem]
  , listings :: mode :- ItemListings.Routes
  }
  deriving stock (Generic)
