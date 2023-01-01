module Muridae.Model.TradableItem where

import DB (muridaeDB)
import DB.Types (muridaeTradableItems)
import DB.Types qualified as DB (TradableItem (TradableItem))
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

import MuridaeWeb.Handler.Item.Types qualified as Handler

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
