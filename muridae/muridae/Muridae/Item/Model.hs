module Muridae.Item.Model (module Muridae.Item.Model) where

import DB (muridaeDB)
import DB.Types (muridaeTradableItems)
import Data.Coerce (coerce)
import Data.Functor.Identity (Identity)
import Database.Beam.Postgres (Pg)
import Database.Beam.Query
  ( all_
  , default_
  , filter_
  , insert
  , insertExpressions
  , runInsert
  , runSelectReturningList
  , runSelectReturningOne
  , select
  , val_
  , (==.)
  )
import Muridae.Item.Types (ItemId (ItemId), Item (Item, _id))
import MuridaeWeb.Handler.Item.Types qualified as Handler

all :: Pg [Item Identity]
all = runSelectReturningList (select (all_ (muridaeDB.muridaeTradableItems)))

create :: Handler.ReqItem -> Pg ()
create tradableItem = do
  runInsert . insert (muridaeDB.muridaeTradableItems) $
    insertExpressions
      [ Item
          default_
          (val_ tradableItem.name)
          (val_ tradableItem.description)
          (val_ tradableItem.wiki_link)
          default_
          (val_ Nothing)
          (val_ Nothing)
      ]

find :: Handler.ItemId -> Pg (Maybe (Item Identity))
find itemId =
  runSelectReturningOne . select $
    filter_ (\item -> item._id ==. val_ (coerce itemId)) $
      all_ (muridaeDB.muridaeTradableItems)
