module Muridae.JSON.Item.Id where

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Servant.API (FromHttpApiData, ToHttpApiData)

newtype ItemId = ItemId Int64
  deriving stock (Show)
  deriving (ToJSON, FromJSON, FromHttpApiData, ToHttpApiData, Eq) via Int64
