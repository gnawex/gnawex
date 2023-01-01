module DB.Types where

import Database.Beam (Database, TableEntity)
import Database.Beam.Postgres (
  Postgres,
 )
import GHC.Generics (Generic)
import Muridae.Item.Types (Item)
import Muridae.ItemListing.Types (ItemListing)

data MuridaeDB f = MuridaeDB
  { muridaeTradableItems :: f (TableEntity Item)
  , muridaeTradableItemListings :: f (TableEntity ItemListing)
  }
  deriving stock (Generic)
  deriving anyclass (Database Postgres)
