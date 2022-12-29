{-# LANGUAGE OverloadedRecordDot #-}

module Muridae.DB.TradableItem where

import Data.Functor.Identity (Identity)
import Database.Beam.Postgres (Pg)
import Database.Beam.Query (
  all_,
  default_,
  insert,
  insertExpressions,
  runInsert,
  runSelectReturningList,
  select,
  val_,
 )
import Muridae.DB (muridaeDB)
import Muridae.DB.Types (muridaeTradableItems)
import qualified Muridae.DB.Types as DB (TradableItem (TradableItem))

import qualified MuridaeWeb.Handlers.Items.Types as Handler

all :: Pg [DB.TradableItem Identity]
all = runSelectReturningList (select (all_ (muridaeTradableItems muridaeDB)))

create :: Handler.ReqTradableItem -> Pg ()
create tradableItem = do
  runInsert . insert (muridaeTradableItems muridaeDB) $
    insertExpressions
      [ DB.TradableItem
          default_
          (val_ tradableItem.name)
          (val_ tradableItem.description)
          (val_ tradableItem.wiki_link)
          default_
          (val_ Nothing)
          (val_ Nothing)
      ]
