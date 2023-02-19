module MuridaeWeb.Handler.Item (indexItems) where

--------------------------------------------------------------------------------

import Data.Vector (Vector)
import Effectful.Servant (runUVerb, throwUVerb)
import Muridae.Item (runManageUserDB)
import Muridae.Item qualified as Item
import MuridaeWeb.Handler.Item.Types qualified as Handler
import MuridaeWeb.JSON.DbError (DbError (DbError))
import MuridaeWeb.Types (Handler')
import Servant (Union, respond)
import MuridaeWeb.Handler.Item.Types (parseItem)

--------------------------------------------------------------------------------

-- create
--   :: Handler.ReqItem
--   -> Handler' (Union '[WithStatus 201 NoContent, WithStatus 500 DbError])
-- create params =
--   runUVerb $
--     runErrorNoCallStack @DbError (Item.create_ params)
--       >>= either
--         (throwUVerb . WithStatus @500)
--         (\_ -> respond (WithStatus @201 NoContent))

indexItems :: Handler' (Union '[Vector Handler.Item, DbError])
indexItems =
  runUVerb $
    runManageUserDB Item.indexItems
      >>= either (throwUVerb . DbError) (respond . fmap parseItem)

-- showDetails
--   :: Handler.ItemId
--   -> Handler'
--       ( Union
--           '[ WithStatus 200 ItemDetails
--            , WithStatus 404 String
--            , WithStatus 500 DbError
--            ]
--       )
-- showDetails itemId =
--   runUVerb $
--     runErrorNoCallStack @DbError (Item.findDetails_ itemId)
--       >>= either
--         (throwUVerb . WithStatus @500)
--         ( maybe
--             ( throwUVerb @(WithStatus 404 String)
--                 (WithStatus @404 "Item does not exist")
--             )
--             (respond @(WithStatus 200 ItemDetails) . WithStatus @200 . fromItem)
--         )

-- getListings
--   :: Handler.ItemId
--   -> Maybe Handler.ItemListingType
--   -> Handler'
--       ( Union
--           '[ WithStatus 200 [ItemListing]
--            , WithStatus 500 DbError
--            ]
--       )
-- getListings itemId itemListingType =
--   runUVerb $
--     runErrorNoCallStack @DbError
--       ( ItemListing.itemListings
--           (coerce itemId)
--           (maybe ByBoth filterListingType itemListingType)
--       )
--       >>= either
--         ( \e -> do
--             _ <- (\(DbError s) -> liftIO $ print s) e
--             throwUVerb $ WithStatus @500 e
--         )
--         (respond @(WithStatus 200 [ItemListing]) . WithStatus @200)
--  where
--   filterListingType :: Handler.ItemListingType -> FilterItemListingType
--   filterListingType = \case
--     Handler.BUY -> ByBuy
--     Handler.SELL -> BySell
