{-# LANGUAGE FieldSelectors #-}

module Muridae.API.Route.Item (module Muridae.API.Route.Item) where

import Data.Vector (Vector)
import GHC.Generics (Generic)
import Muridae.JSON.Item.Id (ItemId)
import Muridae.JSON.Item.Types (Item, ItemDetails)
import Servant (Capture, JSON, UVerb, WithStatus, (:>))
import Servant.API.Generic (type (:-))
import Servant.API.NamedRoutes (NamedRoutes)
import Servant.API.Verbs (StdMethod (GET))
import Muridae.JSON.DbError (DbError)

type Routes = NamedRoutes Routes'

data Routes' mode = Routes'
  { index
      :: mode
        :- UVerb
            'GET
            '[JSON]
            '[Vector Item, WithStatus 500 DbError]
  , show
      :: mode
        :- Capture "item_id" ItemId
        :> UVerb
            'GET
            '[JSON]
            '[ItemDetails, WithStatus 404 String, WithStatus 500 DbError]
  }
  deriving stock (Generic)
