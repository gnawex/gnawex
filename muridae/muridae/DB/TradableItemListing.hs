module DB.TradableItemListing where

import DB (muridaeDB)
import qualified DB.Types as DB
import Data.Functor.Identity (Identity)
import Database.Beam.Postgres (Pg)
import Database.Beam.Query (all_, runSelectReturningList, select)

all :: Pg [DB.TradableItemListing Identity]
all = runSelectReturningList (select (all_ (DB.muridaeTradableItemListings muridaeDB)))
