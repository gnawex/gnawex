module Muridae.Listing where

import DB.Types qualified as DB
import Data.Functor.Identity (Identity)
import Effectful (Eff, Effect, type (:>))
import Effectful.Beam (DB, queryDebug)
import Muridae.Model.TradableItemListing qualified as TradableItemListing

all :: forall (es :: [Effect]). (DB :> es) => Eff es [DB.TradableItemListing Identity]
all = queryDebug print TradableItemListing.all
