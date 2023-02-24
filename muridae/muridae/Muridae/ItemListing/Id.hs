module Muridae.ItemListing.Id (ItemListingId (..)) where

import Data.Int (Int64)

newtype ItemListingId = ItemListingId Int64
  deriving stock (Eq, Show)
