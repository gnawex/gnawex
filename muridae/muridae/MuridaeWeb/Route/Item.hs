{-# LANGUAGE FieldSelectors #-}

module MuridaeWeb.Route.Item (module MuridaeWeb.Route.Item) where

import Data.Vector (Vector)
import GHC.Generics (Generic)
import MuridaeWeb.JSON.DbError (DbError)
import MuridaeWeb.JSON.Item (Item, ItemDetails, ItemId)
import Servant (Capture, JSON, UVerb, (:>), WithStatus)
import Servant.API.Generic (type (:-))
import Servant.API.NamedRoutes (NamedRoutes)
import Servant.API.Verbs (StdMethod (GET))

type Routes = NamedRoutes Routes'

data Routes' mode = Routes'
  { index
      :: mode
        :- UVerb
            'GET
            '[JSON]
            '[Vector Item, DbError]
  , show
      :: mode
        :- Capture "item_id" ItemId
        :> UVerb
            'GET
            '[JSON]
            '[ItemDetails, WithStatus 404 String, DbError]
            -- -- TODO: Replace with an endpoint that responds with listings, not pooled,
            -- -- with filters for listing type, and ordering.
            -- , getListingsUnderItem
            --     :: mode
            --       :- Capture "item_id" ItemId
            --       :> "listings"
            --       :> QueryParam "type" ItemListingType
            --       -- :> QueryParam "sort_cost_by"
            --       :> UVerb
            --           'GET
            --           '[JSON]
            --           '[ WithStatus 200 [ItemListing]
            --            , WithStatus 500 DbError
            --            ]
  }
  deriving stock (Generic)
