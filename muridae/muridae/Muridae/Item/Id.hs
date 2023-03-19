module Muridae.Item.Id (ItemId (..)) where

import Data.Int (Int64)

newtype ItemId = ItemId Int64
  deriving stock (Eq, Show)
