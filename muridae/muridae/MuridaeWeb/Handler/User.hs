module MuridaeWeb.Handler.User (module MuridaeWeb.Handler.User) where

import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Int (Int32)
import Servant.API (FromHttpApiData, ToHttpApiData)

newtype UserId = UserId Int32
  deriving stock (Eq, Show)
  deriving (FromHttpApiData, ToJSON, FromJSON, ToHttpApiData) via Int32
