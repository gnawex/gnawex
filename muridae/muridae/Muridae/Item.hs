module Muridae.Item (runManageUserDB, indexItems, showItem) where

import Data.Int (Int16, Int32, Int64)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Vector (Vector)
import Effectful (Eff, IOE, liftIO, type (:>))
import Effectful.Dispatch.Dynamic (interpret, send)
import Muridae.DB (DB, UsageError)
import Muridae.DB.Item qualified as ItemDB
import Muridae.Item.Types
  ( Item (Item)
  , ItemId (ItemId)
  , ManageItem (IndexItems, ShowItem)
  )
import Muridae.ItemListing (serializePooledBuys, serializePooledSells)
import Muridae.ItemListing.Types (PooledBuyListing, PooledSellListing)

--------------------------------------------------------------------------------

indexItems
  :: (ManageItem :> es, DB :> es)
  => Eff es (Either UsageError (Vector Item))
indexItems = send IndexItems

showItem
  :: (ManageItem :> es, DB :> es)
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

--------------------------------------------------------------------------------
-- Item handler

runManageUserDB
  :: (IOE :> es, DB :> es)
  => Eff (ManageItem : es) (Either UsageError a)
  -> Eff es (Either UsageError a)
runManageUserDB = interpret $ \_ -> \case
  IndexItems ->
    ItemDB.index >>= either (pure . Left) (pure . Right . fmap serializeItem)
  ShowItem (ItemId itemId) ->
    ItemDB.find itemId
      >>= either
        -- FIXME: Use proper logging
        (\e -> liftIO (print e) >> pure (Left e))
        (pure . Right . (serializeItemAndPooledListings =<<))

--------------------------------------------------------------------------------

serializeItemAndPooledListings
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
serializeItemAndPooledListings
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
        ( serializeItem
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
      <*> (Just buys >>= mapM serializePooledBuys)
      <*> (Just sells >>= mapM serializePooledSells)

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
serializeItem
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
