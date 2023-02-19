module Muridae.Item (runManageUserDB, indexItems) where

import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Vector (Vector)
import Effectful (Eff, type (:>))
import Effectful.Dispatch.Dynamic (interpret, send)
import Muridae.DB (DB, UsageError)
import Muridae.DB.Item qualified as ItemDB
import Muridae.Item.Types
  ( Item (Item)
  , ItemId (ItemId)
  , ManageItem (IndexItems)
  )
import Muridae.ItemListing.Types ()

--------------------------------------------------------------------------------

indexItems
  :: (ManageItem :> es, DB :> es)
  => Eff es (Either UsageError (Vector Item))
indexItems = send IndexItems

--------------------------------------------------------------------------------
-- Item handler

runManageUserDB
  :: (DB :> es)
  => Eff (ManageItem : es) (Either UsageError a)
  -> Eff es (Either UsageError a)
runManageUserDB = interpret $ \_ -> \case
  IndexItems ->
    ItemDB.index >>= \case
      Right items -> pure $ Right $ serializeItem <$> items
      Left e -> pure $ Left e

--------------------------------------------------------------------------------

serializeItem
  :: ( Int64
     , Text
     , Text
     , Text
     , UTCTime
     , Maybe UTCTime
     , Maybe UTCTime
     )
  -> Item
serializeItem (itemId, name, wiki, description, createdAt, updatedAt, deletedAt) =
  Item
    (ItemId itemId)
    name
    description
    wiki
    createdAt
    updatedAt
    deletedAt
