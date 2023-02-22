module Muridae.Item.Types (module Muridae.Item.Types) where

import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Vector (Vector)
import Effectful (Dispatch (Dynamic), DispatchOf, Effect)
import Muridae.DB (UsageError)
import Muridae.ItemListing.Types (PooledBuyListing, PooledSellListing)

newtype ItemId = ItemId Int64
  deriving stock (Eq, Show)

newtype ItemName = ItemName Text
  deriving stock (Eq, Show)

newtype ItemDesc = ItemDesc Text
  deriving stock (Eq, Show)

-- TODO: URL type?
newtype ItemWikiLink = ItemWikiLink Text
  deriving stock (Eq, Show)

data Item = Item
  { id :: ItemId
  , name :: Text
  , description :: Text
  , wikiLink :: Text
  , createdAt :: UTCTime
  , updatedAt :: Maybe UTCTime
  , deletedAt :: Maybe UTCTime
  }
  deriving stock (Eq, Show)

data ManageItem :: Effect where
  IndexItems :: ManageItem m (Either UsageError (Vector Item))
  ShowItem
    :: ItemId
    -> ManageItem
        m
        ( Either
            UsageError
            ( Maybe
                ( Item
                , Vector PooledBuyListing
                , Vector PooledSellListing
                )
            )
        )
  CreateItem
    :: ItemName
    -> ItemDesc
    -> ItemWikiLink
    -> ManageItem m (Either UsageError Item)

type instance DispatchOf ManageItem = 'Dynamic
