module Muridae.Item.Types (module Muridae.Item.Types) where

import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Vector (Vector)
import Effectful (Dispatch (Dynamic), DispatchOf, Effect)
import Muridae.DB (UsageError)

newtype ItemId = ItemId Int64
  deriving stock (Eq, Show)

data Item = Item
  { id :: ItemId
  , name :: Text
  , description :: Text
  , wiki :: Text
  , createdAt :: UTCTime
  , updatedAt :: Maybe UTCTime
  , deletedAt :: Maybe UTCTime
  }
  deriving stock (Eq, Show)

data ManageItem :: Effect where
  IndexItems :: ManageItem m (Either UsageError (Vector Item))

type instance DispatchOf ManageItem = 'Dynamic
