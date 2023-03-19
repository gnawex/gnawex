module Muridae.JSON.User where

import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant.API (FromHttpApiData, ToHttpApiData)

newtype UserId = UserId Int64
  deriving stock (Eq, Show)
  deriving (FromHttpApiData, ToJSON, FromJSON, ToHttpApiData) via Int64

data User = User
  { id :: UserId
  , username :: Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)
