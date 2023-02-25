module Muridae.Item (runManageItemDB, indexItems, showItem, createItem) where

import Data.Coerce (coerce)
import Data.Int (Int16, Int32, Int64)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Vector (Vector)
import Effectful (Eff, IOE, liftIO, type (:>))
import Effectful.Dispatch.Dynamic (interpret, send)
import Muridae.DB (DB, UsageError)
import Muridae.DB.Item qualified as ItemDB
import Muridae.Item.Id (ItemId (ItemId))
import Muridae.Item.Types
  ( Item (Item)
  , ItemDesc (ItemDesc)
  , ItemName (ItemName)
  , ItemWikiLink (ItemWikiLink)
  , ManageItem (CreateItem, IndexItems, ShowItem)
  )
import Muridae.ItemListing (parsePooledBuys, parsePooledSells)
import Muridae.ItemListing.Types (PooledBuyListing, PooledSellListing)

--------------------------------------------------------------------------------

indexItems
  :: ManageItem :> es
  => Eff es (Either UsageError (Vector Item))
indexItems = send IndexItems

showItem
  :: ManageItem :> es
  => ItemId
  -> Eff
      es
      ( Either
          UsageError
          ( Maybe
              ( Item
              , Vector PooledBuyListing
              , Vector PooledSellListing
              )
          )
      )
showItem itemId = send (ShowItem itemId)

createItem
  :: (ManageItem :> es)
  => ItemName
  -> ItemDesc
  -> ItemWikiLink
  -> Eff es (Either UsageError Item)
createItem name desc = send . CreateItem name desc

--------------------------------------------------------------------------------
-- Item handler

runManageItemDB
  :: (IOE :> es, DB :> es)
  => Eff (ManageItem : es) (Either UsageError a)
  -> Eff es (Either UsageError a)
runManageItemDB = interpret $ \_ -> \case
  IndexItems ->
    ItemDB.index >>= either (pure . Left) (pure . Right . fmap parseItem)
  ShowItem (ItemId itemId) ->
    ItemDB.find itemId
      >>= either
        -- FIXME: Use proper logging
        (\e -> liftIO (print e) >> pure (Left e))
        (pure . Right . (parseItemAndPooledListings =<<))
  CreateItem name description wikiLink ->
    ItemDB.create (coerce name) (coerce description) (coerce wikiLink)
      >>= either (pure . Left) (pure . Right . parseItem)

--------------------------------------------------------------------------------

parseItemAndPooledListings
  :: ( Int64
     , Text
     , Text
     , Text
     , UTCTime
     , Maybe UTCTime
     , Maybe UTCTime
     , Vector (Int32, Int16, Int64, Scientific)
     , Vector (Int32, Int16, Int64, Scientific)
     )
  -> Maybe (Item, Vector PooledBuyListing, Vector PooledSellListing)
parseItemAndPooledListings
  ( itemId
    , name
    , desc
    , wikiLink
    , createdAt
    , updatedAt
    , deletedAt
    , buys
    , sells
    ) =
    pure (,,)
      <*> Just
        ( parseItem
            ( itemId
            , name
            , desc
            , wikiLink
            , createdAt
            , updatedAt
            , deletedAt
            )
        )
      -- TODO: Change it so that it actually says it failed to parse rather than
      -- say an item does not exist
      <*> (Just buys >>= mapM parsePooledBuys)
      <*> (Just sells >>= mapM parsePooledSells)

parseItem
  :: ( Int64
     , Text
     , Text
     , Text
     , UTCTime
     , Maybe UTCTime
     , Maybe UTCTime
     )
  -> Item
parseItem
  ( itemId
    , name
    , description
    , wikiLink
    , createdAt
    , updatedAt
    , deletedAt
    ) =
    Item
      (ItemId itemId)
      name
      description
      wikiLink
      createdAt
      updatedAt
      deletedAt
