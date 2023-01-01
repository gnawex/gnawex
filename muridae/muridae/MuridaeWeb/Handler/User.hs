module MuridaeWeb.Handler.User where

import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Int (Int32)
import Servant.API (FromHttpApiData)

newtype UserId = UserId Int32
    deriving (FromHttpApiData, ToJSON, FromJSON) via Int32
