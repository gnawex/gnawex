module MuridaeWeb.Handler.Item (indexItems, showItem) where

--------------------------------------------------------------------------------

import Data.Vector (Vector)
import Effectful.Servant (runUVerb, throwUVerb)
import Muridae.Item (runManageUserDB)
import Muridae.Item qualified as Item
import Muridae.Item.Types qualified as Domain
import MuridaeWeb.JSON.DbError (DbError (DbError))
import MuridaeWeb.JSON.Item (ItemDetails, parseItem, parseItemWithPools)
import MuridaeWeb.JSON.Item qualified as JSON
import MuridaeWeb.Types (Handler')
import Servant (Union, WithStatus (WithStatus), respond)

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

-- runErrorNoCallStack @DbError (Item.findDetails_ itemId)
--   >>= either
--     (throwUVerb . WithStatus @500)
--     ( maybe
--         ( throwUVerb @(WithStatus 404 String)
--             (WithStatus @404 "Item does not exist")
--         )
--         (respond @(WithStatus 200 ItemDetails) . WithStatus @200 . fromItem)
--     )

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
