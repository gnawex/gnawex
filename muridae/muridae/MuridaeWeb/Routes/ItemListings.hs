{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators #-}

module MuridaeWeb.Routes.ItemListings where

import GHC.Generics (Generic)
import Servant (JSON)
import Servant.API.Generic (type (:-))
import Servant.API.NamedRoutes (NamedRoutes)
import Servant.API.Verbs (Get)
import MuridaeWeb.Handlers.Items.Listings.Index (TradableItemListing)
import Servant.API.Capture (Capture)
import MuridaeWeb.Handlers.Items.Types (TradableItemId)
import Servant.API (type (:>))

type Routes = NamedRoutes Routes'

data Routes' mode = Routes'
  { index ::
      mode
        :- Capture "item_id" TradableItemId
        :> "listings"
        :> Get '[JSON] [TradableItemListing]
  }
  deriving stock (Generic)
