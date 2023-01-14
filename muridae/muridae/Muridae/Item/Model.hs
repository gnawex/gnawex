module Muridae.Item.Model where

import DB (muridaeDB)
import DB.Types (muridaeTradableItems)
import Data.Functor.Identity (Identity)
import Database.Beam.Postgres (Pg)
import Database.Beam.Query
  ( all_
  , default_
  , insert
  , insertExpressions
  , runInsert
  , runSelectReturningList
  , select
  , val_
  )
import Muridae.Item.Types (Item (Item))
import MuridaeWeb.Handler.Item.Types qualified as Handler

all :: Pg [Item Identity]
all = runSelectReturningList (select (all_ (muridaeDB.muridaeTradableItems)))

create :: Handler.ReqTradableItem -> Pg ()
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
