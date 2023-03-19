module Muridae.API.Handler.Item (indexItems, showItem, createItem) where

--------------------------------------------------------------------------------

import Data.Coerce (coerce)
import Data.Vector (Vector)
import Effectful (liftIO)
import Effectful.Servant (runUVerb, throwUVerb)
import Muridae.JSON.DbError (DbError (DbError))
import Muridae.API.Types (Handler')
import Muridae.Item (runManageItemDB)
import Muridae.Item qualified as Item
import Muridae.Item.Id qualified as Domain (ItemId (ItemId))
import Muridae.Item.Types qualified as Domain
  ( ItemDesc (ItemDesc)
  , ItemName (ItemName)
  , ItemWikiLink (ItemWikiLink)
  )
import Muridae.JSON.Item qualified as JSON
import Muridae.JSON.Item.Id qualified as JSON
import Muridae.JSON.Item.Types qualified as JSON
  ( Item
  , ItemDetails
  , ReqItem (ReqItem)
  )
import Servant
  ( Union
  , WithStatus (WithStatus)
  , respond
  )

--------------------------------------------------------------------------------

createItem
  :: JSON.ReqItem
  -> Handler' (Union '[WithStatus 201 JSON.Item, WithStatus 500 DbError])
createItem (JSON.ReqItem name desc wikiLink) =
  runUVerb $
    runManageItemDB
      ( Item.createItem
          (coerce name)
          (coerce desc)
          (coerce wikiLink)
      )
      >>= either
        (\e -> liftIO (print e) >> (throwUVerb . WithStatus @500 . DbError) e)
        (respond . WithStatus @201 . JSON.serializeItem)

indexItems :: Handler' (Union '[Vector JSON.Item, WithStatus 500 DbError])
indexItems =
  runUVerb $
    runManageItemDB Item.indexItems
      >>= either (throwUVerb . WithStatus @500 . DbError) (respond . fmap JSON.serializeItem)

showItem
  :: JSON.ItemId
  -> Handler'
      ( Union
          '[ JSON.ItemDetails
           , WithStatus 404 String
           , WithStatus 500 DbError
           ]
      )
showItem (JSON.ItemId itemId) =
  runUVerb $
    runManageItemDB (Item.showItem (Domain.ItemId itemId))
      >>= either
        (throwUVerb . WithStatus @500 . DbError)
        ( maybe
            (throwUVerb (WithStatus @404 @String "Item does not exist"))
            (respond . JSON.serializeItemWithPools)
        )
