{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module MuridaeWeb.Routes (
  NamedAPI,
  API (API, publicRoutes),
  PublicRoutes (PublicRoutes, items),
  api,
) where

import Data.Data (Proxy (Proxy))
import GHC.Generics (Generic)
import Servant.API (type (:-), type (:>))
import Servant.API.Generic (ToServantApi, genericApi)
import Servant.API.NamedRoutes (NamedRoutes)
import qualified MuridaeWeb.Routes.Items as Items

type NamedAPI = NamedRoutes API

data API mode = API
  { publicRoutes :: mode :- "api" :> NamedRoutes PublicRoutes
  }
  deriving stock (Generic)

data PublicRoutes mode = PublicRoutes
  { items :: mode :- "items" :> Items.Routes
  }
  deriving stock (Generic)

api :: Proxy (ToServantApi API)
api = genericApi (Proxy :: Proxy API)
