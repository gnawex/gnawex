module DB.Types where

import Database.Beam (Database, TableEntity)
import Database.Beam.Postgres (
  Postgres,
 )
import GHC.Generics (Generic)
import Muridae.Item.Types (Item)
import Muridae.ItemListing.Types (ItemListing)
import Muridae.ItemTxn.Types (ItemTxn)

data MuridaeDB f = MuridaeDB
  { muridaeTradableItems :: f (TableEntity Item)
  , muridaeTradableItemListings :: f (TableEntity ItemListing)
  , muridaeTradableItemTransactions :: f (TableEntity ItemTxn)
  }
  deriving stock (Generic)
  deriving anyclass (Database Postgres)
