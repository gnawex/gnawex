{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators #-}

module MuridaeWeb.Routes.Items where

import GHC.Generics (Generic)
import MuridaeWeb.Handlers.Items.Index (Item)
import Servant (JSON)
import Servant.API.Generic (type (:-))
import Servant.API.NamedRoutes (NamedRoutes)
import Servant.API.Verbs (Get)

type Routes = NamedRoutes Routes'

data Routes' mode = Routes'
  { -- GET /
    index :: mode :- Get '[JSON] [Item]
  }
  deriving stock (Generic)