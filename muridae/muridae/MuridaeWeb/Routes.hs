{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module MuridaeWeb.Routes (
  NamedAPI,
  API (..),
  PublicRoutes (..),
  AdminRoutes (..),
  api,
) where

import Data.Data (Proxy (Proxy))
import GHC.Generics (Generic)
import qualified MuridaeWeb.Routes.Admin.Items as AdminItems
import qualified MuridaeWeb.Routes.Items as Items
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
  { items :: mode :- "items" :> Items.Routes
  }
  deriving stock (Generic)

data AdminRoutes mode = AdminRoutes
  { items :: mode :- "items" :> AdminItems.Routes
  }
  deriving stock (Generic)

api :: Proxy (ToServantApi API)
api = genericApi (Proxy :: Proxy API)
