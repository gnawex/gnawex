module Muridae.API.Route.ItemTxn (module Muridae.API.Route.ItemTxn) where

import Servant (JSON, NamedRoutes, StdMethod (GET, PATCH), UVerb, (:-))

type Routes = NamedRoutes Routes'

data Routes' mode = Routes'
  { index :: mode :- UVerb 'GET '[JSON] '[()]
  -- ^ TODO: Implement
  , updateStatus :: mode :- UVerb 'PATCH '[JSON] '[()]
  -- ^ TODO: Implement
  }
