module MuridaeWeb.Handler.Item (indexItems, showItem, createItem) where

--------------------------------------------------------------------------------

import Data.Coerce (coerce)
import Data.Vector (Vector)
import Effectful (liftIO)
import Effectful.Servant (runUVerb, throwUVerb)
import Muridae.Item (runManageUserDB)
import Muridae.Item qualified as Item
import Muridae.Item.Types qualified as Domain
import MuridaeWeb.JSON.DbError (DbError (DbError))
import MuridaeWeb.JSON.Item
  ( Item
  , ItemDetails
  , ReqItem (ReqItem)
  , parseItem
  , parseItemWithPools
  )
import MuridaeWeb.JSON.Item qualified as JSON
import MuridaeWeb.Types (Handler')
import Servant
  ( Union
  , WithStatus (WithStatus)
  , respond
  )

--------------------------------------------------------------------------------

createItem
  :: JSON.ReqItem
  -> Handler' (Union '[WithStatus 201 Item, DbError])
createItem (ReqItem name desc wikiLink) =
  runUVerb $
    runManageUserDB
      ( Item.createItem
          (coerce name)
          (coerce desc)
          (coerce wikiLink)
      )
      >>= either
        (\e -> liftIO (print e) >> throwUVerb (DbError e))
        (respond . WithStatus @201 . parseItem)

indexItems :: Handler' (Union '[Vector JSON.Item, DbError])
indexItems =
  runUVerb $
    runManageUserDB Item.indexItems
      >>= either (throwUVerb . DbError) (respond . fmap parseItem)

showItem
  :: JSON.ItemId
  -> Handler'
      ( Union
          '[ ItemDetails
           , WithStatus 404 String
           , DbError
           ]
      )
showItem (JSON.ItemId itemId) =
  runUVerb $
    runManageUserDB (Item.showItem (Domain.ItemId itemId))
      >>= either
        (throwUVerb . DbError)
        ( maybe
            (throwUVerb (WithStatus @404 @String "Item does not exist"))
            (respond . parseItemWithPools)
        )
